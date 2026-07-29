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
| `date`, `time`, `interval`, `inet` | `string` |

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

Note that `jsonb` is a normalised representation: PostgreSQL reorders object
keys and drops duplicates, so a round-trip preserves the value, not the text.
Use `json` if you need the text preserved exactly.

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

## Transactions

```ocaml
Sqlml.transaction conn (fun tx ->
  let* _ = create_user tx ~id ~email () in
  set_display_name tx ~id ~display_name ())
```

Commits when the body returns `Ok`, rolls back on `Error` or on an exception,
which is re-raised. Generated functions take the transaction handle unchanged.

## Drivers

`sqlml-postgresql` gives you a single connection over libpq:

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

## Development

```
docker compose up -d     # PostgreSQL 18 on 127.0.0.1:55432
dune build
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

See [ROADMAP.md](ROADMAP.md). Next up is structured error codes, so callers can
tell a unique violation from a deadlock.

## Prior art

[sqlc](https://sqlc.dev) for the authoring model, [sqlgg](https://ygrek.org/p/sqlgg/)
as the closest OCaml ancestor, [PG'OCaml](https://github.com/darioteixeira/pgocaml)
for the idea of typing queries against a live database, and
[Caqti](https://github.com/paurkedal/ocaml-caqti) underneath the pooled driver.

## License

MIT
