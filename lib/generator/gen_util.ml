(** Result plumbing shared across the generator. *)

let ( let* ) = Result.bind

let rec map_result f = function
  | [] -> Ok []
  | x :: tl ->
      let* y = f x in
      let* rest = map_result f tl in
      Ok (y :: rest)

let rec fold_result f acc = function
  | [] -> Ok acc
  | x :: tl -> ( match f acc x with Ok acc -> fold_result f acc tl | Error _ as e -> e)

(* OCaml reserved words: a column alias or query name that lands on one would
   emit uncompilable code with no diagnostic from sqlml. *)
let ocaml_keywords =
  [
    "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let is_lower_ident s =
  s <> ""
  && (s.[0] = '_' || (s.[0] >= 'a' && s.[0] <= 'z'))
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '_' || c = '\'')
       s
  && not (List.mem s ocaml_keywords)
