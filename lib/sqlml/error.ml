type t =
  | Connect of string
  | Execute of { query : string; sql : string; message : string }
  | Decode of { query : string; column : int; expected : string; got : string }
  | Cardinality of { query : string; expected : string; got : int }

let to_string = function
  | Connect m -> Printf.sprintf "connection failed: %s" m
  | Execute { query; sql; message } ->
    Printf.sprintf "query %s failed: %s\n  sql: %s" query message sql
  | Decode { query; column; expected; got } ->
    Printf.sprintf "query %s: column %d: expected %s, got %s" query column expected got
  | Cardinality { query; expected; got } ->
    Printf.sprintf "query %s: expected %s row(s), got %d" query expected got

let pp fmt e = Format.pp_print_string fmt (to_string e)
