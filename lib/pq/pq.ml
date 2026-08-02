(** Thin OCaml surface over pq_stubs.c. Handles are opaque nativeints; the invariant is
    that every [prepare]/[describe]/[exec] result is [clear]ed. *)

type conn = nativeint
type res = nativeint

external connect : string -> conn = "sqlml_pq_connect"
external connect_ok : conn -> bool = "sqlml_pq_connect_ok"
external error_message : conn -> string = "sqlml_pq_error_message"
external finish : conn -> unit = "sqlml_pq_finish"
external prepare : conn -> string -> string -> res = "sqlml_pq_prepare"
external describe_prepared : conn -> string -> res = "sqlml_pq_describe_prepared"
external exec : conn -> string -> res = "sqlml_pq_exec"

external exec_params : conn -> string -> string option array -> res
  = "sqlml_pq_exec_params"

external cmd_tuples : res -> int = "sqlml_pq_cmd_tuples"
external result_error_field : res -> int -> string = "sqlml_pq_result_error_field"

external exec_prepared : conn -> string -> string option array -> res
  = "sqlml_pq_exec_prepared"

(* libpq's PG_DIAG_* selectors, which are just the ASCII codes. *)
let diag_sqlstate = Char.code 'C'
let diag_message_detail = Char.code 'D'
let diag_message_hint = Char.code 'H'
let diag_constraint_name = Char.code 'n'
let diag_table_name = Char.code 't'
let diag_column_name = Char.code 'c'

let opt_field r f =
  match String.trim (result_error_field r f) with "" -> None | s -> Some s

external result_status : res -> int = "sqlml_pq_result_status"
external result_error : res -> string = "sqlml_pq_result_error"
external clear : res -> unit = "sqlml_pq_clear"
external nparams : res -> int = "sqlml_pq_nparams"
external paramtype : res -> int -> int = "sqlml_pq_paramtype"
external nfields : res -> int = "sqlml_pq_nfields"
external fname : res -> int -> string = "sqlml_pq_fname"
external ftype : res -> int -> int = "sqlml_pq_ftype"
external ftable : res -> int -> int = "sqlml_pq_ftable"
external ftablecol : res -> int -> int = "sqlml_pq_ftablecol"
external ntuples : res -> int = "sqlml_pq_ntuples"
external getvalue : res -> int -> int -> string = "sqlml_pq_getvalue"
external getisnull : res -> int -> int -> bool = "sqlml_pq_getisnull"

(* ExecStatusType *)
let command_ok = 1
let tuples_ok = 2
let ok status = status = command_ok || status = tuples_ok

(* Run [f] on a result and always clear it, even if [f] raises. *)
let with_result r f = Fun.protect ~finally:(fun () -> clear r) (fun () -> f r)

(* Diagnostics collected off a failed result, before it is cleared. *)
type diag = {
  message : string;
  sqlstate : string option;
  detail : string option;
  hint : string option;
  constraint_name : string option;
  table_name : string option;
  column_name : string option;
}

let check r =
  if ok (result_status r) then Ok r
  else
    let d =
      {
        message = String.trim (result_error r);
        sqlstate = opt_field r diag_sqlstate;
        detail = opt_field r diag_message_detail;
        hint = opt_field r diag_message_hint;
        constraint_name = opt_field r diag_constraint_name;
        table_name = opt_field r diag_table_name;
        column_name = opt_field r diag_column_name;
      }
    in
    clear r;
    Error d

(* A catalog query returning rows as string arrays; NULL becomes "". *)
let query conn sql =
  match check (exec conn sql) with
  | Error e -> Error e
  | Ok r ->
      with_result r (fun r ->
          let cols = nfields r in
          let rows =
            List.init (ntuples r) (fun i ->
                Array.init cols (fun j -> if getisnull r i j then "" else getvalue r i j))
          in
          Ok rows)

(* Connection string from the conventional environment variables. Shared by the
   generator and the libpq driver so the env policy cannot drift; the Caqti
   driver mirrors it in URI form. *)
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
