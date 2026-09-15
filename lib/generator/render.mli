(** Printing resolved queries as OCaml source. No decisions are made here; anything
    requiring judgement belongs in [Resolve]. *)

val source :
  src:string ->
  enums:(string * string list) list ->
  composites:(string * Resolve.field list) list ->
  rows:(string * Resolve.field list) list ->
  Resolve.resolved list ->
  string * string
(** [(mli, ml)] contents. *)
