type t =
  | Connect of string
  | Execute of
      { query : string
      ; sql : string
      ; message : string
      ; sqlstate : Sqlstate.t option
      ; detail : string option
      ; hint : string option
      ; constraint_name : string option
      ; table_name : string option
      ; column_name : string option
      }
  | Decode of { query : string; column : int; expected : string; got : string }
  | Cardinality of { query : string; expected : string; got : int }

let to_string = function
  | Connect m -> Printf.sprintf "connection failed: %s" m
  | Execute { query; sql; message; sqlstate; detail; constraint_name; _ } ->
    let code = match sqlstate with
      | None -> ""
      | Some s -> Printf.sprintf " [%s %s]" (Sqlstate.to_string s) (Sqlstate.name s)
    in
    let extra =
      String.concat ""
        [ (match constraint_name with None -> "" | Some c -> "\n  constraint: " ^ c)
        ; (match detail with None -> "" | Some d -> "\n  detail: " ^ d)
        ]
    in
    Printf.sprintf "query %s failed%s: %s%s\n  sql: %s" query code message extra sql
  | Decode { query; column; expected; got } ->
    Printf.sprintf "query %s: column %d: expected %s, got %s" query column expected got
  | Cardinality { query; expected; got } ->
    Printf.sprintf "query %s: expected %s row(s), got %d" query expected got

let pp fmt e = Format.pp_print_string fmt (to_string e)

(* Accessors, so callers do not have to destructure a constructor that may grow
   more fields. *)
let sqlstate = function Execute { sqlstate; _ } -> sqlstate | _ -> None
let constraint_name = function Execute { constraint_name; _ } -> constraint_name | _ -> None
let detail = function Execute { detail; _ } -> detail | _ -> None

let is_retryable e =
  match sqlstate e with Some s -> Sqlstate.is_retryable s | None -> false

let is_unique_violation e =
  match sqlstate e with Some s -> Sqlstate.is_unique_violation s | None -> false
