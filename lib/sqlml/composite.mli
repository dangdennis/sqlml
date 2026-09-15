val of_string : string -> string option array
(** A named composite's text representation. Empty unquoted fields are SQL NULL. *)

val to_string : string option array -> string
val field : (string -> 'a) -> string option array -> int -> 'a option
