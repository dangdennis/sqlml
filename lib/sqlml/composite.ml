let of_string s =
  let n = String.length s in
  if n < 2 || s.[0] <> '(' || s.[n - 1] <> ')' then
    invalid_arg "composite: expected parentheses";
  let p = ref 1 in
  let rec fields acc =
    let raw, quoted = Pg_text.token s p ~stop:(fun c -> c = ',' || c = ')') in
    let value = if raw = "" && not quoted then None else Some raw in
    if !p = n - 1 then Array.of_list (List.rev (value :: acc))
    else if !p < n - 1 && s.[!p] = ',' then (
      incr p;
      fields (value :: acc))
    else invalid_arg "composite: trailing or malformed text"
  in
  fields []

let to_string xs =
  "("
  ^ String.concat ","
      (Array.to_list (Array.map (function None -> "" | Some s -> Pg_text.quote s) xs))
  ^ ")"

let field parse xs i =
  if i < 0 || i >= Array.length xs then invalid_arg "composite: missing field";
  Option.map
    (fun s -> Pg_text.context (Printf.sprintf "composite field %d" i) (fun () -> parse s))
    xs.(i)
