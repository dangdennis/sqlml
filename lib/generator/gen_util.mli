(** Result plumbing shared across the generator. *)

val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val map_result : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result

val fold_result :
  ('acc -> 'a -> ('acc, 'e) result) -> 'acc -> 'a list -> ('acc, 'e) result

val ocaml_keywords : string list

val is_lower_ident : string -> bool
(** A valid, non-keyword OCaml lowercase identifier — what a generated field, function or
    type name must be. *)
