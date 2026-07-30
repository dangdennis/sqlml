(** The emit phase: resolve, then render. *)

val generate :
  ?config:Config.t ->
  src:string ->
  Describe.described list ->
  (string * string, Diag.t) result
(** [(mli, ml)] contents for one generated module. *)
