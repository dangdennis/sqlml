(** PostgreSQL type to OCaml type, plus the decoder and encoder expressions the renderer
    splices into generated code. *)

type custom = { c_ocaml : string; c_of_string : string; c_to_string : string }
(** A user-supplied mapping from [sqlml.toml]. *)

type t =
  | Int
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
  | Array of t
  | Option of t

val ocaml_type : t -> string

val decoder : t -> string
(** A function of type [Sqlml.Row.t -> int -> _]. *)

val encoder : t -> string
(** A function of type [_ -> Sqlml.Value.t]. *)

val is_option : t -> bool
val strip_option : t -> t

val of_pg :
  ?custom:custom ->
  type_name:string ->
  elem_type_name:string option ->
  enum_labels:string list ->
  nullable:bool ->
  unit ->
  t option
(** [None] when sqlml has no mapping for the type — reported as a hard error by the
    caller, never a silent fallback. *)

val constructor_of_label : string -> string
(** ['active'] becomes [Active], ['in-progress'] becomes [In_progress]. *)
