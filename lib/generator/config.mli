(** Optional [sqlml.toml] beside the queries directory: renames for generated names, and
    mappings from columns or whole Postgres types to user OCaml types. See the README for
    the file format. *)

type custom = { ocaml : string; of_string : string; to_string : string }
type t

val empty : t

val load : string -> (t, Diag.t) result
(** [load dir] reads [dir/sqlml.toml]; missing file is [Ok empty]. *)

val renamed : t -> string -> string option
(** Lookup for a table name ([users]) or field key ([users.display_name]). *)

val custom : t -> key:string option -> pg_type:string -> custom option
(** Most specific wins: an exact ["table.column"] or ["Query.param"] key before a bare
    Postgres type name. *)

val schema : t -> string list
(** [schema = ["../schema.sql"]]: files (relative to the queries directory) whose hash is
    stored in the offline snapshot, so schema edits invalidate it. *)

val qualify : t -> names:string list -> (t, Diag.t) result
(** Resolve unqualified configuration shorthands, rejecting ambiguity. *)
