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

(* A connection handle.

   This is deliberately NOT parameterised by a phantom region tag. The intent
   was for [transaction] to hand back a distinct [tx conn] so that a nested
   transaction became a type error. It cannot be done here: the tag does not
   generalise through the modular-explicit query functions, and a binding whose
   type contains a modular-explicit arrow cannot carry an explicit polymorphic
   annotation at all -- both ['k.] and [type k.] are rejected with "the
   universal variable would escape its scope" -- so there is no way to force it.

   The bug the tag was meant to prevent is using an outer handle inside a
   transaction body and silently running on a different connection. That is only
   reachable once pooling exists, and the fix there does not need phantom types:
   make the pool a distinct type carrying no query operations, so that obtaining
   a connection at all requires going through [transaction] or
   [with_connection]. *)
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
      | Error (d : Driver.error) ->
          Error
            (Error.Execute
               {
                 query = name;
                 sql;
                 message = d.Driver.message;
                 sqlstate = d.Driver.sqlstate;
                 detail = d.Driver.detail;
                 hint = d.Driver.hint;
                 constraint_name = d.Driver.constraint_name;
                 table_name = d.Driver.table_name;
                 column_name = d.Driver.column_name;
               }))

let fetch_all (module Q : Query.MANY) (conn : conn) (p : Q.params) :
    (Q.row list, Error.t) result =
  match
    run_query conn ~name:Q.name ~sql:Q.sql ~params:(Q.encode p) ~columns:Q.columns
  with
  | Error e -> Error e
  | Ok rows -> ( try Ok (List.map Q.decode rows) with e -> Error (decode_fail Q.name e))

let fetch_one (module Q : Query.ONE) (conn : conn) (p : Q.params) :
    (Q.row option, Error.t) result =
  match
    run_query conn ~name:Q.name ~sql:Q.sql ~params:(Q.encode p) ~columns:Q.columns
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
      match D.exec c ~sql:Q.sql ~params:(Q.encode p) with
      | Ok n -> Ok n
      | Error (d : Driver.error) ->
          Error
            (Error.Execute
               {
                 query = Q.name;
                 sql = Q.sql;
                 message = d.Driver.message;
                 sqlstate = d.Driver.sqlstate;
                 detail = d.Driver.detail;
                 hint = d.Driver.hint;
                 constraint_name = d.Driver.constraint_name;
                 table_name = d.Driver.table_name;
                 column_name = d.Driver.column_name;
               }))

(* ---------- transactions ---------- *)

let statement (conn : conn) sql =
  match conn with
  | Driver.Conn ((module D), c, _) -> (
      match D.exec c ~sql ~params:[] with
      | Ok _ -> Ok ()
      | Error (d : Driver.error) ->
          Error
            (Error.Execute
               {
                 query = "transaction";
                 sql;
                 message = d.Driver.message;
                 sqlstate = d.Driver.sqlstate;
                 detail = d.Driver.detail;
                 hint = d.Driver.hint;
                 constraint_name = d.Driver.constraint_name;
                 table_name = d.Driver.table_name;
                 column_name = d.Driver.column_name;
               }))

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
