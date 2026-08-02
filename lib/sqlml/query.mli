(** The module types generated query modules satisfy, one per cardinality.

    The witnesses make cardinality a compile-time property: [ONE] and [MANY] would
    otherwise be structurally identical, so [fetch_all] on a [:one] query would typecheck.
    Requiring a differently-typed [cardinality] value in each signature is what makes
    misuse uncompilable. *)

type one
type one_strict
type many
type exec
type copy

type _ card =
  | One : one card
  | One_strict : one_strict card
  | Many : many card
  | Exec : exec card
  | Copy : copy card

module type BASE = sig
  type params

  val name : string

  val sql : params -> string
  (** Static queries ignore the argument; a query with optional blocks selects the
      pre-verified variant for which optionals are set. *)

  val encode : params -> Value.t list
end

module type ONE = sig
  include BASE

  type row

  val decode : Row.t -> row
  val columns : int
  val cardinality : one card
end

module type ONE_STRICT = sig
  include BASE

  type row

  val decode : Row.t -> row
  val columns : int
  val cardinality : one_strict card
end

module type MANY = sig
  include BASE

  type row

  val decode : Row.t -> row
  val columns : int
  val cardinality : many card
end

module type EXEC = sig
  include BASE

  val cardinality : exec card
end

module type COPY = sig
  type params
  (** One row of the COPY stream. *)

  val name : string

  val copy_sql : string
  (** [COPY t (a, b) FROM STDIN], built at codegen from the column list the verified
      INSERT names. *)

  val encode : params -> Value.t list
  val cardinality : copy card
end
