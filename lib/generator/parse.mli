(** Parsing [.sql] files into named queries.

    sqlc's authoring model: many queries per file, each introduced by
    [-- name: GetUser :one]. Named parameters ([:id]) are rewritten to positional [$n];
    [/*? ... */] optional blocks are assembled into one pre-rewritten SQL text per
    inclusion combination.

    The records are [private]: fields are readable everywhere, but the parser is the only
    constructor, so a [t] in hand always satisfies the invariants ([params] sorted by
    index, a bounded block count, block parameters disjoint and never nullable,
    [2^nblocks] variants). *)

type cardinality = One | One_strict | Many | Exec | Copy
type param = private { pname : string; index : int; nullable : bool }

type dyn = private {
  nblocks : int;
  variant_sqls : string array;
  variant_nparams : int array;
  block_params : string list array;
}
(** Optional-block data, bitmask-indexed: bit [k] of a variant's index says block [k] is
    included. *)

type t = private {
  name : string;  (** as written: [GetUser] *)
  module_name : string;  (** [Get_user] *)
  cardinality : cardinality;
  doc : string list;
  sql : string;  (** full variant, [$n] placeholders *)
  params : param list;  (** ordered by index *)
  dynamic : dyn option;
  file : string;
  line : int;
}

val of_string : file:string -> string -> (t list, Diag.t) result
val of_file : string -> (t list, Diag.t) result
val to_snake : string -> string
val string_of_cardinality : cardinality -> string
