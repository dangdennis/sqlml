(* What a generated query module looks like from the runtime's point of view.

   One module per named query in a .sql file. The cardinality annotation
   (:one / :many / :exec) selects which of these signatures the generated module
   is checked against, which is what makes [fetch_one] vs [fetch_all] vs [exec]
   a compile-time distinction rather than a runtime flag. *)

module type BASE = sig
  type params

  val name : string
  val sql : string
  val encode : params -> Value.t list
end

(* -- name: GetUser :one *)
module type ONE = sig
  include BASE

  type row

  val decode : Row.t -> row
end

(* -- name: SearchUsers :many *)
module type MANY = sig
  include BASE

  type row

  val decode : Row.t -> row
end

(* -- name: DeleteUser :exec *)
module type EXEC = BASE
