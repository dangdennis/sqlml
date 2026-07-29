(** PostgreSQL driver built directly on libpq.

    A single connection, no pooling. Suited to CLI tools, scripts and tests; a
    web application wants [sqlml-caqti] instead.

    Unlike the Caqti driver, this one reports real affected-row counts. *)

(** Connection string from [DATABASE_URL], or from
    [PGHOST]/[PGPORT]/[PGUSER]/[PGDATABASE]/[PGPASSWORD]. *)
val conninfo_of_env : unit -> string

(** Open a connection. Not closed automatically; use {!with_connection} when the
    lifetime is scoped. *)
val connect : string -> (Sqlml.conn, Sqlml.Error.t) result

(** [with_connection conninfo f] opens a connection, runs [f], and closes it. *)
val with_connection : string -> (Sqlml.conn -> 'a) -> ('a, Sqlml.Error.t) result
