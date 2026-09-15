type dimension = { lower : int; length : int }
(** PostgreSQL arrays, including dimensions, lower bounds, and NULL elements. *)

type 'a t

val make : dimensions:dimension list -> 'a option list -> 'a t
(** Raises [Invalid_argument] for inconsistent dimensions or element counts. *)

val dimensions : 'a t -> dimension list
val elements : 'a t -> 'a option list
val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list
(** Raises [Invalid_argument] unless empty or one-dimensional, lower-bound one, with no
    NULL elements. *)

val of_string : ?delimiter:char -> (string -> 'a) -> string -> 'a t
val to_string : ?delimiter:char -> ('a -> string) -> 'a t -> string
