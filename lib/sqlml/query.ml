(** What a generated query module looks like from the runtime's point of view.

    One module per named query in a .sql file. *)

(* Cardinality witnesses.

   Without these, ONE and MANY are structurally identical module types and EXEC
   is a subset of both, so [fetch_all (module Get_user)] and [exec (module Get_user)] both
   typecheck against a query declared :one -- the annotation in the .sql file
   would be a comment rather than a constraint. Requiring a differently-typed
   witness value in each signature is what makes the cardinality real. *)

(* pure phantom tags; the mli keeps them abstract *)
type one = |
type one_strict = |
type many = |
type exec = |

type _ card =
  | One : one card
  | One_strict : one_strict card
  | Many : many card
  | Exec : exec card

module type BASE = sig
  type params

  val name : string

  (* The SQL text to execute for these parameters. Static queries ignore the
     argument; a query with /*? ... */ optional blocks selects the pre-built,
     codegen-verified variant matching which optional parameters are set. *)
  val sql : params -> string
  val encode : params -> Value.t list
end

(* -- name: GetUser :one *)
module type ONE = sig
  include BASE

  type row

  val decode : Row.t -> row

  (* Number of result columns. Known at codegen time, and required by drivers
     that must declare the row shape before executing -- Caqti builds a static
     row type, unlike libpq which reports the shape back with the result. *)
  val columns : int
  val cardinality : one card
end

(* -- name: GetUser :one! -- the row must exist; absence is an error *)
module type ONE_STRICT = sig
  include BASE

  type row

  val decode : Row.t -> row
  val columns : int
  val cardinality : one_strict card
end

(* -- name: SearchUsers :many *)
module type MANY = sig
  include BASE

  type row

  val decode : Row.t -> row
  val columns : int
  val cardinality : many card
end

(* -- name: DeleteUser :exec *)
module type EXEC = sig
  include BASE

  val cardinality : exec card
end
