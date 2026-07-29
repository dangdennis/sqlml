(* Postgres driver for the sqlml runtime.

   Implements Sqlml.Driver.S over libpq. Nothing above Driver.S knows this
   exists -- generated code takes a [Sqlml.conn] and never names a backend --
   so a Caqti-backed driver (for pooling, and for SQLite) can be added later as
   a second implementation of the same signature without touching a line of
   generated or application code.

   Everything moves in Postgres text format. Values come back as [Value.Text]
   or [Value.Null] and are parsed by [Sqlml.Row], which is why those decoders
   accept text. *)

let hex_of_octets s =
  let b = Buffer.create ((String.length s * 2) + 2) in
  Buffer.add_string b "\\x";
  String.iter (fun c -> Buffer.add_string b (Printf.sprintf "%02x" (Char.code c))) s;
  Buffer.contents b

let text_of_value : Sqlml.Value.t -> string option = function
  | Sqlml.Value.Null -> None
  | Sqlml.Value.Bool b -> Some (if b then "t" else "f")
  | Sqlml.Value.Int n -> Some (string_of_int n)
  (* %.17g round-trips a double exactly *)
  | Sqlml.Value.Float f -> Some (Printf.sprintf "%.17g" f)
  | Sqlml.Value.Text s -> Some s
  | Sqlml.Value.Octets s -> Some (hex_of_octets s)

(* ---------- prepared-statement cache ----------

   Each distinct SQL text is prepared once per connection and executed by name
   afterwards, so the server parses and plans a query once instead of on every
   call. Two realities shape the implementation:

   - Not every statement can be prepared: PREPARE covers SELECT / INSERT /
     UPDATE / DELETE / MERGE / VALUES, and nothing else -- BEGIN, SAVEPOINT,
     DECLARE, FETCH all refuse. Rather than pattern-match SQL, a failed prepare
     marks the text [Unpreparable] and it runs through PQexecParams forever.
     Correctness is unaffected either way; this cache is performance only.

   - A statement prepared inside a transaction dies with that transaction's
     rollback, while the cache entry survives. The next execution then fails
     with 26000 invalid_sql_statement_name -- so that code (and 0A000, "cached
     plan must not change result type", after DDL) re-prepares and retries
     once. *)

module Raw = struct
  type prep = Prepared of string | Unpreparable
  type conn = { raw : Pq.conn; stmts : (string, prep) Hashtbl.t; mutable counter : int }

  let name = "postgresql"
  let placeholder n = "$" ^ string_of_int n

  (* peek at a result's failure state without consuming it *)
  let stale_statement r =
    (not (Pq.ok (Pq.result_status r)))
    &&
    match Pq.opt_field r Pq.diag_sqlstate with
    | Some ("26000" | "0A000") -> true
    | None | Some _ -> false

  let run conn sql params =
    let args = Array.of_list (List.map text_of_value params) in
    let exec_direct () = Pq.exec_params conn.raw sql args in
    let prepare_as stmt =
      let r = Pq.prepare conn.raw stmt sql in
      match Pq.check r with
      | Ok r ->
          Pq.clear r;
          true
      | Error _ -> false
    in
    match Hashtbl.find_opt conn.stmts sql with
    | Some Unpreparable -> exec_direct ()
    | Some (Prepared stmt) ->
        let r = Pq.exec_prepared conn.raw stmt args in
        (* Only the two cache-staleness codes are intercepted; any other
           result, success or failure, passes through untouched so the caller
           diagnoses the original error -- re-running inside an aborted
           transaction would mask it with 25P02. *)
        if stale_statement r then begin
          Pq.clear r;
          if prepare_as stmt then Pq.exec_prepared conn.raw stmt args
          else begin
            Hashtbl.replace conn.stmts sql Unpreparable;
            exec_direct ()
          end
        end
        else r
    | None ->
        conn.counter <- conn.counter + 1;
        let stmt = Printf.sprintf "sqlml_s%d" conn.counter in
        if prepare_as stmt then begin
          Hashtbl.replace conn.stmts sql (Prepared stmt);
          Pq.exec_prepared conn.raw stmt args
        end
        else begin
          Hashtbl.replace conn.stmts sql Unpreparable;
          exec_direct ()
        end

  (* libpq reports the column count back with the result, so [columns] is
     redundant here. It exists for drivers that must declare the shape up
     front. *)
  let of_diag (d : Pq.diag) =
    Sqlml.Driver.error d.Pq.message
      ?sqlstate:(Option.map Sqlml.Sqlstate.of_string d.Pq.sqlstate)
      ?detail:d.Pq.detail ?hint:d.Pq.hint ?constraint_name:d.Pq.constraint_name
      ?table_name:d.Pq.table_name ?column_name:d.Pq.column_name

  let query conn ~sql ~params ~columns:_ =
    match Pq.check (run conn sql params) with
    | Error e -> Error (of_diag e)
    | Ok r ->
        Pq.with_result r (fun r ->
            let cols = Pq.nfields r in
            Ok
              (List.init (Pq.ntuples r) (fun i ->
                   Array.init cols (fun j ->
                       if Pq.getisnull r i j then Sqlml.Value.Null
                       else Sqlml.Value.Text (Pq.getvalue r i j)))))

  let exec conn ~sql ~params =
    match Pq.check (run conn sql params) with
    | Error e -> Error (of_diag e)
    | Ok r -> Pq.with_result r (fun r -> Ok (Pq.cmd_tuples r))
end

(* ---------- connecting ---------- *)

let conninfo_of_env () =
  match Sys.getenv_opt "DATABASE_URL" with
  | Some u when String.trim u <> "" -> u
  | _ ->
      let get k d = match Sys.getenv_opt k with Some v when v <> "" -> v | _ -> d in
      Printf.sprintf "host=%s port=%s user=%s dbname=%s%s" (get "PGHOST" "127.0.0.1")
        (get "PGPORT" "5432") (get "PGUSER" "postgres") (get "PGDATABASE" "postgres")
        (match Sys.getenv_opt "PGPASSWORD" with
        | Some p when p <> "" -> " password=" ^ p
        | _ -> "")

let open_raw conninfo =
  let c = Pq.connect conninfo in
  if Pq.connect_ok c then Ok { Raw.raw = c; stmts = Hashtbl.create 16; counter = 0 }
  else
    let m = String.trim (Pq.error_message c) in
    Pq.finish c;
    Error (Sqlml.Error.Connect m)

(* Does not close. Intended for a connection that lives as long as the process;
   use [with_connection] when the lifetime is scoped. *)
let connect conninfo =
  Result.map (fun c -> Sqlml.Driver.make (module Raw) c) (open_raw conninfo)

let with_connection conninfo f =
  match open_raw conninfo with
  | Error e -> Error e
  | Ok c ->
      Fun.protect
        ~finally:(fun () -> Pq.finish c.Raw.raw)
        (fun () -> Ok (f (Sqlml.Driver.make (module Raw) c)))
