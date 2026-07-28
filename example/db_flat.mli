(** SHAPE B -- flat types, query modules hidden entirely.

    Same queries, same runtime, different exposed surface. The [Query.ONE]
    modules still exist inside db_flat.ml -- they just aren't exported, so the
    signature collapses to types and functions. Compare the size of this file
    to db.mli: this is the whole public API. *)

type user_status =
  | Active
  | Banned

val user_status_to_string : user_status -> string

type get_user_row =
  { id : string
  ; email : string
  ; display_name : string option
  ; status : user_status
  ; created_at : string
  }

(** Fetch a single user by id. *)
val get_user : Sqlml.conn -> id:string -> (get_user_row option, Sqlml.Error.t) result

type search_users_row =
  { id : string
  ; email : string
  ; created_at : string
  }

(** Users in an organization whose email matches a pattern, newest first. *)
val search_users :
  Sqlml.conn ->
  organization_id:string ->
  email_pattern:string ->
  limit:int ->
  (search_users_row list, Sqlml.Error.t) result

val delete_user : Sqlml.conn -> id:string -> (int, Sqlml.Error.t) result
