(** PostgreSQL [interval] values: three independent fields, because a month has no fixed
    length and a day is not always 24 hours. Parsing accepts all four [IntervalStyle]
    output formats; printing emits a unit-spelled form the server accepts under any style.
*)

type t = { months : int; days : int; micros : int64 }

val zero : t

val make :
  ?years:int ->
  ?months:int ->
  ?days:int ->
  ?hours:int ->
  ?minutes:int ->
  ?seconds:int ->
  ?micros:int64 ->
  unit ->
  t

val equal : t -> t -> bool
val to_string : t -> string
val pp : Format.formatter -> t -> unit

exception Bad_interval of string

val of_string : string -> t
(** @raise Bad_interval on anything PostgreSQL would not print. *)

val of_string_opt : string -> t option
