(** The one error type of the generator pipeline: every failure from parse to emit reports
    through this, so all messages share the [file:line: Query: message] shape and
    improving diagnostics means editing one printer. *)

type t = {
  file : string option;
  line : int option;
  query : string option;
  message : string;
}

val v : ?file:string -> ?line:int -> ?query:string -> string -> t

val error :
  ?file:string ->
  ?line:int ->
  ?query:string ->
  ('a, unit, string, ('b, t) result) format4 ->
  'a
(** [error ?file ?line ?query fmt ...] builds an [Error] directly. *)

val to_string : t -> string
