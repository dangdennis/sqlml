(* sqlml runtime -- public interface.

   The whole point of this file is the three execution functions below. Each
   takes a generated query module as a *modular explicit* argument, so the
   module you pass determines both the parameter type you must supply and the
   result type you get back:

     val fetch_one : (module Q : Query.ONE) -> Driver.t -> Q.params -> (Q.row option, _) result

   With ordinary first-class modules this type is not writable in curried form:
   [Q.params] and [Q.row] would each need to escape as a separate type variable
   threaded through a [with type params = 'p and type row = 'r] constraint, and
   every caller would have to reconstruct that witness. Here they simply project
   out of the module argument.

   Call sites look like:

     let user = Sqlml.fetch_one (module Db.Get_user) conn { id = 42 }
     let users = Sqlml.fetch_all (module Db.Search_users) conn { org_id; limit = 100 }
     let n = Sqlml.exec (module Db.Delete_user) conn { id = 42 }

   The module argument is written by the code generator in typical use, so the
   explicitness costs the user nothing -- which is exactly why *explicits* are a
   better fit here than implicits. *)

module Value = Value
module Error = Error
module Row = Row
module Driver = Driver
module Query = Query
module Sqlstate = Sqlstate
module Interval = Interval

(* A connection handle. Deliberately NOT parameterised by a phantom region
   tag: the tag cannot generalise through the modular-explicit query
   functions. Full postmortem in DESIGN.md, "Transactions, and the phantom tag
   that died twice". *)
type conn = Driver.t

(* Generated code emits a raising wrapper and a [_res] wrapper per query:

     val get_user     : conn -> id:Uuidm.t -> get_user_row option
     val get_user_res : conn -> id:Uuidm.t -> (get_user_row option, Error.t) result

   The raising one is [or_raise] applied to the other. *)
exception Sql_error of Error.t

let or_raise = function Ok v -> v | Error e -> raise (Sql_error e)

let () =
  Printexc.register_printer (function
    | Sql_error e -> Some ("Sqlml.Sql_error: " ^ Error.to_string e)
    | _ -> None)

(* Every driver diagnostic becomes an Error.Execute here, so the mapping
   between the two error shapes exists in exactly one place. *)
let execute_error ~query ~sql (d : Driver.error) =
  Error.Execute
    {
      query;
      sql;
      message = d.Driver.message;
      sqlstate = d.Driver.sqlstate;
      detail = d.Driver.detail;
      hint = d.Driver.hint;
      constraint_name = d.Driver.constraint_name;
      table_name = d.Driver.table_name;
      column_name = d.Driver.column_name;
    }

let decode_fail name (e : exn) =
  match e with
  | Row.Bad { column; expected; got } ->
      Error.Decode { query = name; column; expected; got }
  | e -> raise e

let run_query (conn : conn) ~name ~sql ~params ~columns =
  match conn with
  | Driver.Conn ((module D), c, _) -> (
      match D.query c ~sql ~params ~columns with
      | Ok rows -> Ok rows
      | Error (d : Driver.error) -> Error (execute_error ~query:name ~sql d))

let fetch_all (module Q : Query.MANY) (conn : conn) (p : Q.params) :
    (Q.row list, Error.t) result =
  match
    run_query conn ~name:Q.name ~sql:(Q.sql p) ~params:(Q.encode p) ~columns:Q.columns
  with
  | Error e -> Error e
  | Ok rows -> ( try Ok (List.map Q.decode rows) with e -> Error (decode_fail Q.name e))

let fetch_one (module Q : Query.ONE) (conn : conn) (p : Q.params) :
    (Q.row option, Error.t) result =
  match
    run_query conn ~name:Q.name ~sql:(Q.sql p) ~params:(Q.encode p) ~columns:Q.columns
  with
  | Error e -> Error e
  | Ok [] -> Ok None
  | Ok [ r ] -> ( try Ok (Some (Q.decode r)) with e -> Error (decode_fail Q.name e))
  | Ok rows ->
      Error
        (Error.Cardinality
           { query = Q.name; expected = "at most 1"; got = List.length rows })

let exec (module Q : Query.EXEC) (conn : conn) (p : Q.params) : (int, Error.t) result =
  match conn with
  | Driver.Conn ((module D), c, _) -> (
      match D.exec c ~sql:(Q.sql p) ~params:(Q.encode p) with
      | Ok n -> Ok n
      | Error (d : Driver.error) -> Error (execute_error ~query:Q.name ~sql:(Q.sql p) d))

let copy (module Q : Query.COPY) (conn : conn) (rows : Q.params list) :
    (int, Error.t) result =
  match conn with
  | Driver.Conn ((module D), c, _) -> (
      let lines = List.map (fun r -> Value.Copy.line (Q.encode r)) rows in
      match D.copy c ~sql:Q.copy_sql ~rows:lines with
      | Ok n -> Ok n
      | Error (d : Driver.error) -> Error (execute_error ~query:Q.name ~sql:Q.copy_sql d))

let fetch_one_strict (module Q : Query.ONE_STRICT) (conn : conn) (p : Q.params) :
    (Q.row, Error.t) result =
  match
    run_query conn ~name:Q.name ~sql:(Q.sql p) ~params:(Q.encode p) ~columns:Q.columns
  with
  | Error e -> Error e
  | Ok [] -> Error (Error.Cardinality { query = Q.name; expected = "exactly 1"; got = 0 })
  | Ok [ r ] -> ( try Ok (Q.decode r) with e -> Error (decode_fail Q.name e))
  | Ok rows ->
      Error
        (Error.Cardinality
           { query = Q.name; expected = "exactly 1"; got = List.length rows })

let close (conn : conn) = match conn with Driver.Conn ((module D), c, _) -> D.close c

(* ---------- transactions ---------- *)

let statement (conn : conn) sql =
  match conn with
  | Driver.Conn ((module D), c, _) -> (
      match D.exec c ~sql ~params:[] with
      | Ok _ -> Ok ()
      | Error (d : Driver.error) -> Error (execute_error ~query:"transaction" ~sql d))

(* Commits when [f] returns [Ok], rolls back when it returns [Error] or raises.

   Nesting works via savepoints: a [transaction] on a handle already inside a
   transaction issues SAVEPOINT/RELEASE rather than BEGIN/COMMIT, so an inner
   failure rolls back only the inner work. Depth is tracked on the handle.

   [?retry] re-runs the body on a serialization failure (40001) or deadlock
   (40P01), the standard pattern for [`Serializable]. Only those two: a
   connection failure would fail again on the same dead handle, and anything
   else is deterministic. Retry applies only at the outermost level, because
   PostgreSQL dooms the whole transaction on a serialization failure -- an
   inner body cannot usefully re-run, so the error propagates to the outer
   attempt, which re-runs everything. A raised [Sql_error] with a retryable
   code is treated the same as returning it. *)
let transaction ?isolation ?(retry = 0) (conn : conn) f =
  let (Driver.Conn (_, _, depth)) = conn in
  if !depth > 0 then begin
    if isolation <> None then
      invalid_arg
        "Sqlml.transaction: ~isolation cannot be changed inside an enclosing transaction";
    let sp = Printf.sprintf "sqlml_savepoint_%d" !depth in
    match statement conn ("SAVEPOINT " ^ sp) with
    | Error e -> Error e
    | Ok () -> (
        incr depth;
        let out = match f conn with r -> `Returned r | exception e -> `Raised e in
        decr depth;
        match out with
        | `Returned (Ok v) -> (
            match statement conn ("RELEASE SAVEPOINT " ^ sp) with
            | Ok () -> Ok v
            | Error e -> Error e)
        | `Returned (Error e) ->
            ignore (statement conn ("ROLLBACK TO SAVEPOINT " ^ sp));
            Error e
        | `Raised e ->
            ignore (statement conn ("ROLLBACK TO SAVEPOINT " ^ sp));
            raise e)
  end
  else begin
    let begin_sql =
      match isolation with
      | None -> "BEGIN"
      | Some `Read_committed -> "BEGIN ISOLATION LEVEL READ COMMITTED"
      | Some `Repeatable_read -> "BEGIN ISOLATION LEVEL REPEATABLE READ"
      | Some `Serializable -> "BEGIN ISOLATION LEVEL SERIALIZABLE"
    in
    let retryable e =
      match Error.sqlstate e with
      | Some s -> Sqlstate.is_serialization_failure s
      | None -> false
    in
    let rec attempt remaining =
      match statement conn begin_sql with
      | Error e -> Error e
      | Ok () -> (
          incr depth;
          let out = match f conn with r -> `Returned r | exception e -> `Raised e in
          decr depth;
          match out with
          | `Returned (Ok v) -> (
              match statement conn "COMMIT" with
              | Ok () -> Ok v
              (* a failed COMMIT has already aborted the transaction server-side *)
              | Error e when retryable e && remaining > 0 -> attempt (remaining - 1)
              | Error e -> Error e)
          | `Returned (Error e) ->
              ignore (statement conn "ROLLBACK");
              if retryable e && remaining > 0 then attempt (remaining - 1) else Error e
          | `Raised (Sql_error e) when retryable e && remaining > 0 ->
              ignore (statement conn "ROLLBACK");
              attempt (remaining - 1)
          | `Raised e ->
              ignore (statement conn "ROLLBACK");
              raise e)
    in
    attempt retry
  end

(* ---------- streaming ---------- *)

(* Batched streaming over a server-side cursor, composed entirely from the
   operations Driver.S already has -- DECLARE with the query's own parameters,
   FETCH FORWARD in batches, CLOSE -- so both drivers stream without changes.

   A WITHOUT HOLD cursor needs a transaction; [transaction] provides one, and
   its savepoint nesting means streaming inside a caller's transaction works.
   The fold therefore runs inside a transaction, and its effects roll back if
   the fold raises. *)

let fetch_fold (module Q : Query.MANY) ?(batch = 500) (conn : conn) (p : Q.params)
    ~(init : 'acc) ~(f : 'acc -> Q.row -> 'acc) : ('acc, Error.t) result =
  if batch <= 0 then invalid_arg "Sqlml.fetch_fold: ~batch must be positive";
  transaction conn @@ fun tx ->
  (* The cursor name is derived from transaction depth, not a global counter:
     statements on one connection are sequential per depth, so the name set is
     finite and the libpq statement cache cannot grow one entry per call. *)
  let (Driver.Conn (_, _, depth)) = tx in
  let cur = Printf.sprintf "sqlml_cursor_d%d" !depth in
  let declare () =
    match tx with
    | Driver.Conn ((module D), c, _) -> (
        match
          D.exec c
            ~sql:(Printf.sprintf "DECLARE %s NO SCROLL CURSOR FOR %s" cur (Q.sql p))
            ~params:(Q.encode p)
        with
        | Ok _ -> Ok ()
        | Error (d : Driver.error) -> Error (execute_error ~query:Q.name ~sql:(Q.sql p) d)
        )
  in
  let fetch_sql = Printf.sprintf "FETCH FORWARD %d FROM %s" batch cur in
  let rec loop acc =
    match run_query tx ~name:Q.name ~sql:fetch_sql ~params:[] ~columns:Q.columns with
    | Error e -> Error e
    | Ok [] -> Ok acc
    | Ok rows -> (
        match List.fold_left (fun a r -> f a (Q.decode r)) acc rows with
        | acc -> if List.length rows < batch then Ok acc else loop acc
        | exception e -> Error (decode_fail Q.name e))
  in
  match declare () with
  | Error e -> Error e
  | Ok () -> (
      let out = loop init in
      match statement tx ("CLOSE " ^ cur) with
      | Ok () -> out
      | Error e -> ( match out with Error _ -> out | Ok _ -> Error e))

let fetch_iter (module Q : Query.MANY) ?batch (conn : conn) (p : Q.params)
    ~(f : Q.row -> unit) : (unit, Error.t) result =
  fetch_fold (module Q) ?batch conn p ~init:() ~f:(fun () r -> f r)
