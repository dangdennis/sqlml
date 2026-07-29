(** Caqti driver over Eio, with connection pooling.

    This is the driver for a web application. Eio is direct style, so generated signatures
    are unchanged — a query returns a plain [result], not a promise.

    Errors carry a SQLSTATE, so failures can be classified. They do not carry the
    constraint name, detail or hint: Caqti's Postgres driver exposes only the message and
    the code. Use [sqlml-postgresql] if you need to know which constraint was violated.

    {[
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pool =
      Result.get_ok
        (Sqlml_caqti.Pool.create ~sw
           ~stdenv:(env :> Caqti_eio.stdenv)
           ~max_size:10 (Sqlml_caqti.uri_of_env ()))
    in
    Sqlml_caqti.Pool.use pool (fun conn -> Db.get_user conn ~id)
    ]} *)

val uri_of_env : unit -> Uri.t
(** Connection URI from [DATABASE_URL], or from [PGHOST]/[PGPORT]/[PGUSER]/[PGDATABASE].
*)

val connect :
  sw:Eio.Switch.t ->
  stdenv:Caqti_eio.stdenv ->
  Uri.t ->
  (Sqlml.conn, Sqlml.Error.t) result
(** A single pooled-less connection, scoped to the switch. Prefer {!Pool}. *)

module Pool : sig
  type t
  (** A pool carries no query operations on purpose. The only way to reach a {!Sqlml.conn}
      is {!use} or {!transaction}, both of which scope it, so a connection cannot outlive
      its borrow and a transaction cannot leak statements onto a different connection. *)

  val create :
    sw:Eio.Switch.t ->
    stdenv:Caqti_eio.stdenv ->
    ?max_size:int ->
    Uri.t ->
    (t, Sqlml.Error.t) result

  val use : t -> (Sqlml.conn -> ('a, Sqlml.Error.t) result) -> ('a, Sqlml.Error.t) result
  (** Borrow a connection for the duration of [f]. *)

  val transaction :
    t -> (Sqlml.conn -> ('a, Sqlml.Error.t) result) -> ('a, Sqlml.Error.t) result
  (** Borrow a connection and run [f] on it inside a transaction. Pinning to one
      connection is why this belongs on the pool rather than being assembled by the
      caller. See {!Sqlml.transaction} for commit and rollback behaviour. *)
end
