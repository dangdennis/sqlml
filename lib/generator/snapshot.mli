(** A committed cache of what {!Describe} learned from the database, so [generate] and
    [check] can run [--offline] -- in CI jobs without Postgres, or on a laptop on a plane.

    [sqlml.snapshot.json] lives beside the queries. Each query entry is keyed on a hash of
    {e every} SQL variant it can execute, so editing any query invalidates its entry
    loudly rather than serving stale types; a global hash over the schema files listed in
    [sqlml.toml] ([schema = ["../schema.sql"]]) catches schema edits the same way. Schema
    drift {e outside} the listed files is invisible offline by construction -- a live
    [check] in CI remains the backstop. *)

val filename : string
(** [sqlml.snapshot.json] *)

val write :
  queries_dir:string ->
  config:Config.t ->
  Describe.described list ->
  (string, Diag.t) result
(** Serializes to [queries_dir/sqlml.snapshot.json] (sorted, so diffs stay minimal) and
    returns the path. *)

val describe_offline :
  queries_dir:string ->
  config:Config.t ->
  Parse.t list ->
  (Describe.described list, Diag.t) result
(** The offline stand-in for {!Describe.describe_all}: every query must be present with
    identical variant hashes and the schema hash must match, or the error names the query
    (or file) and says to re-run [sqlml snapshot]. *)
