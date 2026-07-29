(** Thin OCaml surface over pq_stubs.c. Handles are opaque nativeints; the
   invariant is that every [prepare]/[describe]/[exec] result is [clear]ed. *)

type conn = nativeint
type res = nativeint

external connect : string -> conn = "sqlml_pq_connect"
external connect_ok : conn -> bool = "sqlml_pq_connect_ok"
external error_message : conn -> string = "sqlml_pq_error_message"
external finish : conn -> unit = "sqlml_pq_finish"
external prepare : conn -> string -> string -> res = "sqlml_pq_prepare"
external describe_prepared : conn -> string -> res = "sqlml_pq_describe_prepared"
external exec : conn -> string -> res = "sqlml_pq_exec"
external exec_params : conn -> string -> string option array -> res = "sqlml_pq_exec_params"
external cmd_tuples : res -> int = "sqlml_pq_cmd_tuples"
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
external fmod : res -> int -> int = "sqlml_pq_fmod"
external ntuples : res -> int = "sqlml_pq_ntuples"
external getvalue : res -> int -> int -> string = "sqlml_pq_getvalue"
external getisnull : res -> int -> int -> bool = "sqlml_pq_getisnull"

(* ExecStatusType *)
let command_ok = 1
let tuples_ok = 2

let ok status = status = command_ok || status = tuples_ok

(* Run [f] on a result and always clear it, even if [f] raises. *)
let with_result r f =
  Fun.protect ~finally:(fun () -> clear r) (fun () -> f r)

let check r =
  if ok (result_status r) then Ok r
  else (
    let e = String.trim (result_error r) in
    clear r;
    Error e)

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
