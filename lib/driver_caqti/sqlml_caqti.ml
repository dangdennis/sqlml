(* Caqti driver for the sqlml runtime, over Eio.

   Two things make this worth having over the libpq driver: connection pooling,
   and a concurrency story. Eio is direct style, so nothing in [Driver.S] or in
   any generated signature changes — a query still returns a plain [result], not
   a promise.

   Everything crosses the boundary as text. Caqti's Postgres driver sends
   parameters with unspecified type OIDs and lets the server infer them, so a
   [uuid], [numeric] or enum parameter works with no cast — verified, not
   assumed. Values come back as [Value.Text] / [Value.Null] and are parsed by
   [Sqlml.Row], which is why those decoders accept text. *)

(* ---------- dynamic Caqti types ----------

   Caqti's codecs are static, but sqlml's model is dynamic: a list of values in,
   an array of values out, with the shape known only at codegen time. Nesting
   [t2] existentially bridges the two. Nesting rather than [tup3]/[tup4] also
   means there is no arity ceiling — a 40-column row is no harder than a 3-column
   one. *)

type row_t = Row : 'a Caqti_type.t * ('a -> Sqlml.Value.t list) -> row_t

let text = Caqti_type.(option string)

let rec row_type n =
  if n <= 0 then Row (Caqti_type.unit, fun () -> [])
  else
    match row_type (n - 1) with
    | Row (t, f) ->
        Row
          ( Caqti_type.t2 text t,
            fun (x, rest) ->
              (match x with None -> Sqlml.Value.Null | Some s -> Sqlml.Value.Text s)
              :: f rest )

type arg_t = Arg : 'a Caqti_type.t * (string option list -> 'a) -> arg_t

let rec arg_type n =
  if n <= 0 then Arg (Caqti_type.unit, fun _ -> ())
  else
    match arg_type (n - 1) with
    | Arg (t, f) ->
        Arg
          ( Caqti_type.t2 text t,
            fun l -> match l with x :: tl -> (x, f tl) | [] -> (None, f []) )

(* ---------- the driver ---------- *)

(* Caqti's own [cause] enumeration is deliberately incomplete, but the Postgres
   driver carries the raw SQLSTATE in its [Result_error_msg], so failures are
   still classified.

   Note the asymmetry with the libpq driver: Result_error_msg carries only the
   message and the SQLSTATE, so detail, hint, constraint name, table and column
   are all None here. If you need to know *which* constraint was violated
   rather than merely that one was, use sqlml-postgresql. *)
let diag_of_caqti (e : [< Caqti_error.t ]) =
  let fallback () = Sqlml.Driver.error (Caqti_error.show e) in
  match e with
  | `Request_failed qe | `Response_failed qe -> (
      match qe.Caqti_error.msg with
      | Caqti_driver_postgresql.Result_error_msg { error_message; sqlstate } ->
          Sqlml.Driver.error error_message
            ?sqlstate:
              (match String.trim sqlstate with
              | "" -> None
              | s -> Some (Sqlml.Sqlstate.of_string s))
      | _ -> fallback ())
  | _ -> fallback ()

module Raw = struct
  type conn = (module Caqti_eio.CONNECTION)

  let name = "caqti-eio/postgresql"
  let placeholder n = "$" ^ string_of_int n

  let query (module Db : Caqti_eio.CONNECTION) ~sql ~params ~columns =
    let (Arg (at, mk)) = arg_type (List.length params) in
    let (Row (rt, get)) = row_type columns in
    (* ~oneshot: a fresh request value is created per call, and a non-oneshot
       request would register a new entry in Caqti's per-connection prepared
       cache every time -- unbounded growth for no reuse. The libpq driver is
       the one with a real statement cache. *)
    let req =
      Caqti_request.create ~oneshot:true at rt Caqti_mult.zero_or_more (fun _ ->
          Caqti_query.of_string_exn sql)
    in
    match Db.collect_list req (mk (List.map Sqlml.Value.to_pg_text params)) with
    | Ok rows -> Ok (List.map (fun r -> Array.of_list (get r)) rows)
    | Error e -> Error (diag_of_caqti e)

  (* Caqti's exec reports no affected-row count, so this returns 1 on success.
     The libpq driver returns the real count via PQcmdTuples; if the count
     matters to you, prefer that driver or run a RETURNING query. *)
  let exec (module Db : Caqti_eio.CONNECTION) ~sql ~params =
    let (Arg (at, mk)) = arg_type (List.length params) in
    let req =
      Caqti_request.create ~oneshot:true at Caqti_type.unit Caqti_mult.zero (fun _ ->
          Caqti_query.of_string_exn sql)
    in
    match Db.exec req (mk (List.map Sqlml.Value.to_pg_text params)) with
    | Ok () -> Ok 1
    | Error e -> Error (diag_of_caqti e)
end

let of_connection (c : (module Caqti_eio.CONNECTION)) : Sqlml.conn =
  Sqlml.Driver.make (module Raw) c

(* See the libpq driver: the decoders parse ISO output, so DateStyle is pinned
   per session. Runs once per physical connection via post_connect. *)
let session_setup (module Db : Caqti_eio.CONNECTION) =
  let req =
    Caqti_request.create ~oneshot:true Caqti_type.unit Caqti_type.unit Caqti_mult.zero
      (fun _ -> Caqti_query.of_string_exn "SET datestyle TO ISO")
  in
  Db.exec req ()

let err e = Sqlml.Error.Connect (Caqti_error.show e)

(* ---------- connecting ---------- *)

let uri_of_env () =
  Uri.of_string
    (match Sys.getenv_opt "DATABASE_URL" with
    | Some u when String.trim u <> "" -> u
    | _ ->
        let get k d = match Sys.getenv_opt k with Some v when v <> "" -> v | _ -> d in
        Printf.sprintf "postgresql://%s@%s:%s/%s" (get "PGUSER" "postgres")
          (get "PGHOST" "127.0.0.1") (get "PGPORT" "5432") (get "PGDATABASE" "postgres"))

(* A single connection, for scripts and tests. A web app wants {!connect_pool}. *)
let connect ~sw ~stdenv uri =
  match Caqti_eio_unix.connect ~sw ~stdenv uri with
  | Error e -> Error (err e)
  | Ok c -> (
      match session_setup c with
      | Ok () -> Ok (of_connection c)
      | Error e -> Error (err e))

(* ---------- pooling ----------

   [Pool.t] deliberately carries no query operations. The only way to reach a
   [Sqlml.conn] is {!use} or {!transaction}, both of which scope it — which is
   the enforcement the phantom-typed transaction handle could not give us. It
   needs no type-level machinery, just not exposing the operation. *)

module Pool = struct
  type t = ((module Caqti_eio.CONNECTION), Caqti_error.t) Caqti_eio.Pool.t

  let create ~sw ~stdenv ?max_size uri =
    let pool_config =
      match max_size with
      | None -> None
      | Some n -> Some (Caqti_pool_config.create ~max_size:n ())
    in
    match
      Caqti_eio_unix.connect_pool ~sw ~stdenv ?pool_config ~post_connect:session_setup uri
    with
    | Ok p -> Ok p
    | Error e -> Error (err e)

  (* Borrow a connection for the duration of [f]. [f] returns a result, as every
     generated query function does, and it is flattened rather than nested. *)
  let use pool f =
    match Caqti_eio.Pool.use (fun c -> Ok (f (of_connection c))) pool with
    | Ok inner -> inner
    | Error e -> Error (err e)

  (* Borrow a connection and run [f] inside a transaction on it. Pinning to one
     connection is exactly why this belongs on the pool rather than being
     assembled by the caller. *)
  let transaction ?isolation ?retry pool f =
    use pool (fun conn -> Sqlml.transaction ?isolation ?retry conn f)
end
