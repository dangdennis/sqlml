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

module Raw = struct
  type conn = Pq.conn

  let name = "postgresql"
  let placeholder n = "$" ^ string_of_int n

  let run conn sql params =
    Pq.exec_params conn sql (Array.of_list (List.map text_of_value params))

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
      (match Sys.getenv_opt "PGPASSWORD" with Some p when p <> "" -> " password=" ^ p | _ -> "")

let open_raw conninfo =
  let c = Pq.connect conninfo in
  if Pq.connect_ok c then Ok c
  else (
    let m = String.trim (Pq.error_message c) in
    Pq.finish c;
    Error (Sqlml.Error.Connect m))

(* Does not close. Intended for a connection that lives as long as the process;
   use [with_connection] when the lifetime is scoped. *)
let connect conninfo = Result.map (fun c -> Sqlml.Driver.make (module Raw) c) (open_raw conninfo)

let with_connection conninfo f =
  match open_raw conninfo with
  | Error e -> Error e
  | Ok c ->
    Fun.protect
      ~finally:(fun () -> Pq.finish c)
      (fun () -> Ok (f (Sqlml.Driver.make (module Raw) c)))
