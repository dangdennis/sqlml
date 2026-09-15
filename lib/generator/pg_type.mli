type id = { schema : string; name : string }
(** Database-independent PostgreSQL identities and their transitive catalog graph. *)

type attribute = { name : string; typ : id; number : int; typmod : int }

type kind =
  | Base
  | Enum of string list
  | Domain of { base : id; not_null : bool; typmod : int; constraints : string list }
  | Array of { element : id; delimiter : char }
  | Composite of attribute list
  | Range of id
  | Multirange of id
  | Unsupported of string

type registry = (id * kind) list
type t = { id : id; registry : registry }

val qualified : id -> string
val generated_name : id -> string
val kind : t -> kind
val at : t -> id -> t
val discover : Pq.conn -> int list -> ((int * t) list, Diag.t) result
val to_json : t -> Yojson.Safe.t

val of_json : Yojson.Safe.t -> t
(** Raises [Invalid_argument] on malformed or dangling graphs. *)

val legacy : name:string -> element:string option -> labels:string list -> t
(** Fixture constructor; live inference always uses [discover]. *)
