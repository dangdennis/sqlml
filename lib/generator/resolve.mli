(** Semantic analysis: every decision behind generated code, with rendering left to
    [Render]. Records are [private]: readable by the renderer, constructed only here. *)

type field = private {
  fname : string;
  ftype : Typemap.t;
  ord : int;  (** attnum for a table column, 0 otherwise *)
}

type origin =
  | From_table of string  (** shared model for a table *)
  | From_query of string  (** named after the query *)

type resolved = private {
  d : Describe.described;
  row_type : string option;  (** [None] for [:exec] and [:copy] *)
  row_origin : origin option;
  cols : field list;  (** SELECT order — decoders index by position *)
  type_fields : field list;  (** order the record type is declared in *)
  ps : field list;
  copy : (string * string list) option;
      (** [:copy] target table and column list, validated against the verified INSERT *)
}

val resolve : Config.t -> Describe.described -> (resolved, Diag.t) result

val collect_rows : resolved list -> ((string * field list) list, Diag.t) result
(** Row types to emit, first-seen order, deduplicated by name; a name claimed with
    different fields is an error naming both origins. *)

val collect_enums : resolved list -> ((string * string list) list, Diag.t) result
val block_param_names : Parse.t -> string list
