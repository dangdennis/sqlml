(** PostgreSQL type to OCaml type, plus the decoder and encoder expressions the renderer
    splices into generated code. *)

type custom = { c_ocaml : string; c_of_string : string; c_to_string : string }
(** A user-supplied mapping from [sqlml.toml]. *)

type t =
  | Int
  | Int64
  | Float
  | Bool
  | String
  | Bytes
  | Uuid
  | Ptime
  | Decimal
  | Json
  | Date
  | Time_of_day
  | Interval
  | Enum of string * string list  (** ocaml type name, labels in sort order *)
  | Custom of custom
  | Array of t * char
  | Composite of string
  | Range of t
  | Multirange of t
  | Option of t

val ocaml_type : t -> string

val decoder : t -> string
(** A function of type [Sqlml.Row.t -> int -> _]. *)

val encoder : t -> string
(** A function of type [_ -> Sqlml.Value.t]. *)

val is_option : t -> bool
val strip_option : t -> t

val constructor_of_label : string -> string
(** ['active'] becomes [Active], ['in-progress'] becomes [In_progress]. *)

val elem_parser : t -> string
val elem_printer : t -> string

val of_type :
  ?custom:(Pg_type.id -> custom option) ->
  ?rename:(Pg_type.id -> string) ->
  Pg_type.t ->
  nullable:bool ->
  t option
