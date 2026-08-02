(** The wire-neutral value type exchanged with drivers, and the encoders generated code
    uses. Richer Postgres types ride as [Text]/[Octets] and are parsed by {!Row}, so
    adding a type mapping never requires a driver change. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | Text of string
  | Octets of string

val type_name : t -> string

(** {1 Encoders spliced into generated code} *)

val of_bool : bool -> t
val of_int : int -> t
val of_float : float -> t
val of_string : string -> t
val of_octets : string -> t
val of_uuid : Uuidm.t -> t
val of_decimal : Decimal.t -> t
val of_ptime : Ptime.t -> t
val of_json : Yojson.Safe.t -> t
val of_date : Ptime.date -> t
val of_time_of_day : Ptime.Span.t -> t
val of_interval : Interval.t -> t
val of_option : ('a -> t) -> 'a option -> t

val of_list : ('a -> string) -> 'a list -> t
(** Encodes an array column or parameter as PostgreSQL's [{a,b,c}] literal, quoting
    elements per its rules. *)

(** Element printers for arrays: a cell must be plain text because it is spliced into the
    array literal. *)
module Print : sig
  val string : string -> string
  val octets : string -> string
  val int : int -> string
  val float : float -> string
  val bool : bool -> string
  val uuid : Uuidm.t -> string
  val decimal : Decimal.t -> string
  val ptime : Ptime.t -> string
  val json : Yojson.Safe.t -> string
  val date : Ptime.date -> string
  val time_of_day : Ptime.Span.t -> string
  val interval : Interval.t -> string
end

val to_pg_text : t -> string option
(** The wire encoding both drivers share: PostgreSQL text format, [None] for SQL NULL. One
    home so the drivers cannot drift apart on rendering. *)

(** COPY text-format cells: its own quoting regime, distinct from array literals.
    Backslash-escapes for tab, newline, carriage return and friends; [\N] for NULL. *)
module Copy : sig
  val cell : t -> string

  val line : t list -> string
  (** Tab-joined cells; no trailing newline — the driver owns line framing. *)
end
