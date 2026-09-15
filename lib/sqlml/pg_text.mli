val quote : string -> string
(** Shared PostgreSQL container text grammar primitives. *)

val token : string -> int ref -> stop:(char -> bool) -> string * bool
val context : string -> (unit -> 'a) -> 'a
