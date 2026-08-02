(** Asking PostgreSQL what a query's types are.

    Each query is PQprepared (letting the server infer parameter types — the inference
    {e is} the answer) and PQdescribed, yielding parameter type OIDs and, per result
    column, the name, type OID and originating table column. Everything else is catalog
    lookups over those OIDs. Every variant of a dynamic query is verified, and all
    variants must agree on the result shape.

    Records are [private]: readable everywhere, constructed only here, so a [described] in
    hand reflects what the server actually said. *)

type column = private {
  name : string;  (** alias with any [!]/[?] override stripped *)
  type_oid : int;
  type_name : string;
  elem_type_name : string option;  (** [Some] when the type is an array *)
  table : string option;  (** relname, when the column comes from a table *)
  table_oid : int;  (** 0 when not a plain column reference *)
  table_col : int;  (** attnum; 0 when [table_oid] is 0 *)
  nullable : bool;
  enum_labels : string list;  (** non-empty when the type is an enum *)
}

type param = private {
  index : int;
  pname : string;
  ptype_oid : int;
  ptype_name : string;
  pelem_type_name : string option;
  penum_labels : string list;
  pnullable : bool;  (** from a trailing [?] on the placeholder *)
}

type described = private {
  query : Parse.t;
  params : param list;
  columns : column list;
  model_table : string option;
      (** set when the result is exactly one table's full column set, licensing a shared
          model row type *)
}

val conninfo_of_env : unit -> string
val connect : string -> (Pq.conn, Diag.t) result
val describe_all : Pq.conn -> Parse.t list -> (described list, Diag.t) result

val report : described -> string
(** Human-readable report for one query, as printed by [sqlml describe]. *)
