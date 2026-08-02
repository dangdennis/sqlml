# sqlml

Typed OCaml from your SQL, checked against PostgreSQL.

You write `.sql` files. sqlml asks PostgreSQL for the parameter and result types
of each query, then emits an OCaml module. No query DSL, no ORM, no hand-written
codecs.

## Example

`sql/users.sql`:

```sql
-- name: GetUser :one
-- Fetch a single user by id.
SELECT id, email, display_name, status, created_at
FROM users
WHERE id = :id;

-- name: SearchUsers :many
SELECT id, email FROM users
WHERE organization_id = :organization_id AND email ILIKE :pattern
LIMIT :limit;

-- name: DeleteUser :exec
DELETE FROM users WHERE id = :id;
```

```
$ sqlml generate -q sql -o lib/db
wrote lib/db/db.mli and lib/db/db.ml (3 queries from 1 file(s))
```

The generated interface:

```ocaml
type user_status = Active | Banned

type get_user_row =
  { id : Uuidm.t
  ; email : string
  ; display_name : string option
  ; status : user_status
  ; created_at : Ptime.t
  }

val get_user : Sqlml.conn -> id:Uuidm.t -> (get_user_row option, Sqlml.Error.t) result
val get_user_exn : Sqlml.conn -> id:Uuidm.t -> get_user_row option
```

And using it:

```ocaml
open Db

let () =
  match get_user conn ~id with
  | Ok (Some u) -> print_endline u.email
  | Ok None -> print_endline "not found"
  | Error e -> prerr_endline (Sqlml.Error.to_string e)
```

`display_name` is an `option` because the column is nullable; `status` is a
variant because the column is an enum. Neither was annotated.

## Install

```
opam install sqlml sqlml-postgresql
```

`sqlml` is the runtime that generated code depends on. `sqlml-postgresql`
provides the `sqlml` command and a libpq driver. Add `sqlml-caqti` for
connection pooling.

Requires OCaml 5.5.0 or later, for modular explicits.

## Commands

```
sqlml generate -q sql -o lib/db      write db.ml and db.mli
sqlml check    -q sql -o lib/db      verify they match the database; exit 1 if not
sqlml describe -q sql                print what PostgreSQL says about each query
```

Connection comes from `DATABASE_URL`, or `PGHOST`/`PGPORT`/`PGUSER`/
`PGDATABASE`/`PGPASSWORD`, or `--database`.

`-q` defaults to `sql` and is searched recursively. Every query becomes a nested
module inside one generated module, whichever file it came from, so row types
can be shared across files. Query names must therefore be unique project-wide;
a collision is reported with both locations.

Run `check` in CI. Generated code keeps compiling after the schema moves under
it and only fails at runtime; `check` is what catches that.

```
(rule
 (alias runtest)
 (deps (glob_files_rec sql/*.sql) db.ml db.mli)
 (action (run sqlml check -q sql -o . -m db)))
```

## Annotations

| Syntax | Meaning |
| --- | --- |
| `-- name: GetUser :one` | at most one row; returns `option` |
| `-- name: GetUser :one!` | exactly one row; returned unwrapped, absence is an error |
| `-- name: ListUsers :many` | zero or more rows; returns `list` |
| `-- name: DeleteUser :exec` | no rows; returns the affected count |
| `:id` | named parameter, becomes a labelled argument |
| `:name?` | nullable parameter, becomes an optional argument |
| `AS "total!"` | force a column non-null |
| `AS "title?"` | force a column nullable |

Cardinality is enforced by the type system: passing a `:one` query to
`fetch_all` does not compile.

Nullability is read from the catalog, so most columns need no annotation. The
overrides exist for the two cases the catalog gets wrong: a `NOT NULL` column
reached through an outer join, and a computed column with no originating table.

## Types

| PostgreSQL | OCaml |
| --- | --- |
| `bool` | `bool` |
| `int2`, `int4`, `int8` | `int` |
| `float4`, `float8` | `float` |
| `numeric` | `Decimal.t` |
| `text`, `varchar`, `char` | `string` |
| `bytea` | `string` |
| `uuid` | `Uuidm.t` |
| `timestamp`, `timestamptz` | `Ptime.t` |
| `json`, `jsonb` | `Yojson.Safe.t` |
| enum types | a variant |
| `T[]` | `T list` |
| `date` | `Ptime.date` |
| `time` | `Ptime.Span.t` (since midnight) |
| `interval` | `Sqlml.Interval.t` |
| `timetz`, `inet` | `string` |

An unmapped type is an error naming the column, not a silent fall back to
`string`.

Arrays give you dynamic IN lists:

```sql
-- name: GetUsersByIds :many
SELECT id, email FROM users WHERE id = ANY(:ids);
```

```ocaml
val get_users_by_ids : Sqlml.conn -> ids:Uuidm.t list -> (get_users_by_ids_row list, _) result
```

`timestamptz` decodes to the instant it names: output carries the session's
offset, which the parser honours, so any server `TimeZone` round-trips
correctly. `timestamp` (without time zone) has no zone to honour; it is read
and written as UTC wall-clock time by convention — prefer `timestamptz`. Both
drivers pin `datestyle = ISO` per connection, so a server configured with
`German` or `SQL` output styles cannot poison temporal decoding.

`Sqlml.Interval.t` is `{ months; days; micros }` — three independent fields,
because a month has no fixed length and a day is not always 24 hours. Parsing
accepts all four PostgreSQL `IntervalStyle` output formats.

Note that `jsonb` is a normalised representation: PostgreSQL reorders object
keys and drops duplicates, so a round-trip preserves the value, not the text.
Use `json` if you need the text preserved exactly.

## Configuration

An optional `sqlml.toml` beside your queries directory controls the two things
that cannot be inferred: what generated names are called, and which OCaml type
a column maps to.

```toml
[rename]
users = "user"                  # users_row becomes user_row
"users.display_name" = "name"   # the field becomes `name`

[types."users.id"]              # one column
ocaml = "User_id.t"
of_string = "User_id.of_string"
to_string = "User_id.to_string"

[types.citext]                  # or a whole Postgres type
ocaml = "Email.t"
of_string = "Email.of_string"
to_string = "Email.to_string"
```

Table names are not singularized automatically. English pluralization is a
swamp — `data`, `series`, `status`, `people` — so an explicit rename is longer
and always right.

Parameters are keyed on the query rather than the column, because PostgreSQL
reports a parameter's type but not which column it is compared against, so
`users.id` cannot reach the `$1` in `WHERE id = $1`:

```toml
[types."GetUser.id"]
ocaml = "User_id.t"
of_string = "User_id.of_string"
to_string = "User_id.to_string"
```

## Errors

Failures carry a SQLSTATE, so a handler can act on them:

```ocaml
match create_user conn ~id ~email () with
| Ok _ -> respond `Created
| Error e when Sqlml.Error.is_unique_violation e ->
  respond (`Conflict (Option.value (Sqlml.Error.constraint_name e) ~default:"duplicate"))
| Error e when Sqlml.Error.is_retryable e -> retry ()
| Error e -> log (Sqlml.Error.to_string e); respond `Internal_error
```

`Sqlml.Sqlstate` covers all 262 PostgreSQL codes across 43 classes, with
`condition` for matching, `name`, `class_`, and predicates including
`is_retryable` (serialization failure, deadlock, or connection loss).

The libpq driver also reports the constraint name, detail, hint, table and
column. The Caqti driver reports only the message and code, which is all Caqti
exposes.

## Streaming

For results too large to hold as a list, rows are read in batches from a
server-side cursor, so memory is bounded by the batch size:

```ocaml
Sqlml.fetch_fold (module Db.Search_users) ~batch:1000 conn
  { organization_id; email_pattern = "%"; limit = 1_000_000 }
  ~init:0 ~f:(fun n _row -> n + 1)
```

`fetch_iter` is the same with no accumulator. Streaming uses the exported query
module directly, so parameters are passed as the record rather than labelled
arguments. Works through both drivers and inside an enclosing transaction.

## Optional filters

A `/*? ... */` block is included only when its parameter is supplied:

```sql
-- name: FindUsers :many
SELECT id, email, status FROM users
WHERE organization_id = :org
  /*? AND email ILIKE :email */
  /*? AND status = :status */
ORDER BY email
LIMIT :limit;
```

```ocaml
val find_users :
  Sqlml.conn -> org:Uuidm.t -> limit:int ->
  ?email:string -> ?status:user_status -> unit ->
  (find_users_row list, Sqlml.Error.t) result
```

Every inclusion combination is generated as its own statement and verified
against the database at codegen, so all of them are type-checked, each plans
with only its live predicates, and each gets its own prepared-statement cache
entry. Blocks may appear anywhere in the query — subqueries and CTEs included —
because each combination is validated whole. Blocks must not change the result
shape, and at most 4 blocks are allowed per query (2^n combinations are
verified).

A parameter inside a block must belong to that block alone. A block with
several parameters is included when all of them are supplied; supplying only
some raises `Invalid_argument`.

## Transactions

```ocaml
Sqlml.transaction ~isolation:`Serializable ~retry:3 conn (fun tx ->
  let* balance = get_balance tx ~id in
  set_balance tx ~id ~balance:(debit balance amount))
```

Commits when the body returns `Ok`, rolls back on `Error` or on an exception,
which is re-raised. Generated functions take the transaction handle unchanged.

Nesting uses savepoints, so an inner failure rolls back only the inner work.
`~retry` re-runs the body on a serialization failure or deadlock — the standard
companion to `` `Serializable`` — and on nothing else, since other errors would
fail identically again. The body must be safe to re-run.

## Drivers

`sqlml-postgresql` gives you a single connection over libpq. Each distinct
query is prepared once per connection and executed by name afterwards, so the
server plans it once, not per call:

```ocaml
let conn = Result.get_ok (Sqlml_pg.connect (Sqlml_pg.conninfo_of_env ()))
```

`sqlml-caqti` gives you a pool over Caqti and Eio. Eio is direct style, so
generated signatures do not change:

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let pool =
  Result.get_ok
    (Sqlml_caqti.Pool.create ~sw ~stdenv:(env :> Caqti_eio.stdenv) ~max_size:10
       (Sqlml_caqti.uri_of_env ()))
in
Sqlml_caqti.Pool.use pool (fun conn -> get_user conn ~id)
```

A pool exposes no query operations. Reaching a connection requires `Pool.use` or
`Pool.transaction`, both of which scope it.

## Formatting

Generated code is run through `ocamlformat` using whatever `.ocamlformat`
applies to the output directory, so it arrives in your project's own style.

This is not cosmetic. `check` compares byte-for-byte, so without it an editor
that formats on save would make `check` fail forever on code you did not write.
Formatting in the generator means `generate` and `check` agree by construction.
If `ocamlformat` is not installed, output is emitted unformatted rather than
failing.

## Development

```
docker compose up -d     # PostgreSQL 18 on 127.0.0.1:55432
dune build               # dune package management: fetches deps from dune.lock
dune test                # unit tests, no database
DATABASE_URL=postgresql://sqlml:sqlml@127.0.0.1:55432/sqlml dune exec example/e2e.exe
```

`example/` holds a worked schema, queries, generated output, and three
programs: `app` on a single connection, `web` on a pool, `e2e` covering the
type mappings.

## Limitations

- PostgreSQL only. The driver boundary anticipates SQLite, but SQLite has no
  equivalent of Describe, so inference there needs a different approach.
- Codegen needs a live database. Generated code does not.
- One statement per named query; no dynamic query building beyond `= ANY(...)`.
- Nested arrays are rejected. A NULL array element is a decode error, since
  PostgreSQL does not report whether elements are nullable.
- The Caqti driver reports SQLSTATE but not the constraint name, detail or
  hint; libpq reports all of them.
- `int8` maps to `int`, which is 63-bit. Values beyond that are a decode error
  rather than a silent truncation.

## Roadmap

See [ROADMAP.md](ROADMAP.md). Next up: an offline snapshot mode, so `sqlml
check` can run without a live database, and bulk COPY inserts.

## Prior art

[sqlc](https://sqlc.dev) for the authoring model, [sqlgg](https://ygrek.org/p/sqlgg/)
as the closest OCaml ancestor, [PG'OCaml](https://github.com/darioteixeira/pgocaml)
for the idea of typing queries against a live database, and
[Caqti](https://github.com/paurkedal/ocaml-caqti) underneath the pooled driver.

## License

MIT
