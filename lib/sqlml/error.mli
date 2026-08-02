(** Errors surfaced to applications. Server failures carry the SQLSTATE and diagnostic
    fields when the driver can supply them, so callers can act on a failure — retry a
    serialization failure, 409 a unique violation — rather than parse prose. *)

type t =
  | Connect of string
  | Execute of {
      query : string;
      sql : string;
      message : string;
      sqlstate : Sqlstate.t option;
      detail : string option;
      hint : string option;
      constraint_name : string option;
      table_name : string option;
      column_name : string option;
    }
  | Decode of { query : string; column : int; expected : string; got : string }
  | Cardinality of { query : string; expected : string; got : int }

val to_string : t -> string
val pp : Format.formatter -> t -> unit

(** {1 Accessors} *)

val sqlstate : t -> Sqlstate.t option
val constraint_name : t -> string option
val detail : t -> string option
val is_retryable : t -> bool
val is_unique_violation : t -> bool
