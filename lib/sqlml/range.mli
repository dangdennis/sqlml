(** PostgreSQL range bounds. Canonicalization is performed by the server. *)
type 'a bound = Unbounded | Inclusive of 'a | Exclusive of 'a

type 'a t = Empty | Bounds of { lower : 'a bound; upper : 'a bound }

val of_string : (string -> 'a) -> string -> 'a t
val to_string : ('a -> string) -> 'a t -> string
val multirange_of_string : (string -> 'a) -> string -> 'a t list
val multirange_to_string : ('a -> string) -> 'a t list -> string
