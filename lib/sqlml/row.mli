(** Positional row decoders used by generated code. A decoder reads like the SELECT list
    it came from; failures raise {!Bad}, which the runtime turns into [Error.Decode] so
    generated code stays flat.

    [t] is concretely an array of {!Value.t} so drivers and tests can build rows directly.
*)

type t = Value.t array

exception Bad of { column : int; expected : string; got : string }

val int : t -> int -> int
val bool : t -> int -> bool
val string : t -> int -> string
val octets : t -> int -> string
val float : t -> int -> float
val uuid : t -> int -> Uuidm.t
val ptime : t -> int -> Ptime.t
val decimal : t -> int -> Decimal.t
val json : t -> int -> Yojson.Safe.t
val date : t -> int -> Ptime.date
val time_of_day : t -> int -> Ptime.Span.t
val interval : t -> int -> Interval.t

val option : (t -> int -> 'a) -> t -> int -> 'a option
(** Wraps another decoder: [Row.(option string) r 3]. *)

val custom : (string -> 'a) -> t -> int -> 'a
(** For a column mapped to a user type via sqlml.toml; a raising parser becomes a decode
    error like any other. *)

val list : (string -> 'a) -> t -> int -> 'a list
(** Decodes an array column via an {!Elem} parser. A NULL element is an error: PostgreSQL
    does not report element nullability, so rejecting beats guessing. *)

(** Element parsers for arrays: they receive one element's raw text rather than indexing a
    row. *)
module Elem : sig
  val string : string -> string
  val int : string -> int
  val float : string -> float
  val bool : string -> bool
  val uuid : string -> Uuidm.t
  val decimal : string -> Decimal.t
  val ptime : string -> Ptime.t
  val json : string -> Yojson.Safe.t
  val date : string -> Ptime.date
  val time_of_day : string -> Ptime.Span.t
  val interval : string -> Interval.t
end
