# sqlml — design

SQL-first query compiler for OCaml. You write `.sql` files; `sqlml` asks
PostgreSQL what the types are and emits typed OCaml query modules; a small
runtime executes them.

Lineage: **sqlc** for the authoring model, **sqlgg** as closest OCaml prior art,
**Caqti** as the intended execution substrate (hidden), **modular explicits**
for the execution API.

## Status

| Piece | State |
|---|---|
| `lib/sqlml` — runtime, execution API, driver boundary | **working, tested** |
| `test/` — 20 tests, incl. 14 over real generator output | **passing** |
| `example/` — hand-written target output + call sites | **compiles and runs** |
| `lib/generator/parse.ml` — .sql → named queries, `:id` → `$n` | **working** |
| `lib/generator/pq.ml` + `pq_stubs.c` — libpq binding | **working** |
| `lib/generator/describe.ml` — PG Describe + catalog resolution | **working** |
| `lib/generator/typemap.ml` — Postgres type → OCaml type | **working** |
| `lib/generator/emit.ml` — .ml + .mli emitter | **working** |
| `bin/` — cmdliner CLI: `generate`, `check`, `describe` | **working** |
| `example/generated/` — real generator output, compiled and tested | **passing** |
| `lib/pq` — raw libpq binding (shared) | **working** |
| `lib/driver_pg` — Postgres driver over libpq | **working** |
| `example/e2e.ml` — generated code against real Postgres | **16/16 passing** |
| `lib/driver_caqti` — Caqti/Eio driver with pooling | **working** |
| `example/web.ml` — pooled, concurrent handlers | **working** |

`example/db.mli` is the contract the generator must hit. It is hand-written and
compiles; the generator's job is to produce it byte-for-byte from
`example/sql/users.sql` plus a live database.

## Toolchain

Modular explicits are **not in any released OCaml**. This project requires the
`samsa1/ocaml` fork:

```bash
opam switch create modexp --empty
opam pin add --yes --no-action --switch modexp \
  "ocaml-variants.5.3.0+modular-explicit" \
  "git+https://github.com/samsa1/ocaml#5.3.0+modular-explicit"
opam install --yes --switch modexp "ocaml-variants.5.3.0+modular-explicit" dune
```

Chose `5.3.0+modular-explicit` over `modular-implicits-5.5.1` (which has both
explicits and implicits): explicits are what this design wants, 5.3.0 satisfies
every dependency (`caqti-eio` needs only `ocaml >= 5.0.0`), and it is the least
experimental branch that has the feature.

## Development database

```bash
docker compose up -d     # Postgres 18 on 127.0.0.1:55432, schema auto-applied
```

`example/schema.sql` is mounted into `/docker-entrypoint-initdb.d`, so it runs
on first boot only. After editing it: `docker compose down -v && docker compose up -d`.

Two gotchas, both hit on the way in:

- **Postgres 18 changed the volume convention.** Data lives in a
  major-version subdirectory, so the volume mounts at `/var/lib/postgresql`,
  not `/var/lib/postgresql/data`. Mounting the old path makes the image refuse
  to start with a `pg_ctlcluster` compatibility error.
- Port 55432, not 5432, to stay clear of any local install.

## Why modular explicits here

The execution API:

```ocaml
val fetch_one : {Q : Query.ONE}  -> Driver.t -> Q.params -> (Q.row option, Error.t) result
val fetch_all : {Q : Query.MANY} -> Driver.t -> Q.params -> (Q.row list,   Error.t) result
val exec      : {Q : Query.EXEC} -> Driver.t -> Q.params -> (int,          Error.t) result
```

The module argument determines both the parameter type you must supply and the
result type you get. With ordinary first-class modules this isn't writable in
curried form — `Q.params` and `Q.row` would each have to escape as a type
variable threaded through a `with type params = 'p and type row = 'r` witness
that every caller reconstructs.

**Explicits, not implicits.** The usual objection to explicits is call-site
noise: you write `{Get_user}` every time. But in an sqlc-style tool a *program*
writes those call sites. The verbosity is paid by the generator and the
ergonomic cost to the user is zero — so the inference machinery of implicits
buys nothing, at the cost of a more experimental compiler branch.

**Where explicits are *not* used:** the driver. A connection is an ordinary
value you store in records and pass around, so it's an existential GADT over a
first-class module (`Driver.t`), not a brace argument. Explicits are for the
thing whose types must project into a signature; existentials are for the thing
that must stay a plain value. Using explicits for both would force every call
site to name its driver for no benefit.

## Layering

```
.sql files
   ↓  generator: discover → parse (-- name: X :one) → PG Parse/Describe → typemap → emit
generated query modules  (match Query.ONE / MANY / EXEC)
   ↓  Sqlml.fetch_one {Q} / fetch_all {Q} / exec {Q}
Driver.S  ←— the backend boundary; Caqti lives strictly below it
   ↓
postgres / sqlite
```

`lib/sqlml` has **no dependency on Caqti or Eio**. That's deliberate: Caqti's
tuple codecs and first-class connection modules must never reach a generated
signature, so Caqti can be swapped for a native wire driver without touching a
line of generated application code.

Values crossing the driver boundary are `Value.t` (`Null | Bool | Int | Float |
Text | Octets`). Richer Postgres types — uuid, timestamptz, json, arrays, enums
— ride as `Text`/`Octets` and are converted in generated code via `Row`, so
adding a type mapping never requires a driver change. This also sidesteps
Caqti's tuple-arity ceiling: rows decode positionally, so a 30-column SELECT is
no harder than a 3-column one.

## Two constraints the first build surfaced

Both are now encoded in the passing test, and both are the generator's problem:

1. **Record field collisions.** `params` and `row` in one module routinely share
   field names (`id`, `email`). OCaml resolves an unannotated `{ id }` pattern
   to the *last* type defined, so the generator must emit `let encode ({ id } :
   params) = ...` and `let decode r : row = ...` — annotations, always.

2. **Warning 69 fires in generated code.** A row record's fields are written by
   the decoder but often only some are read by the caller, which trips
   `unused-field` in the *defining* module — i.e. in generated code, for a
   reason the user cannot fix. Generated files must carry `[@@@warning "-69"]`.

## The generated API

Decided by working through three compiling variants in `example/` (see git
history for the alternatives).

**Shape C.** Row types at the top level, query modules also exported. One
`open Db` per file puts every row's fields in scope, so `u.email` works and
resolves by type-directed disambiguation even when several row types share a
field name. Exporting the query module keeps `Sqlml.fetch_one {Db.Get_user}`
available for tooling that wants to be generic over queries.

**Labelled arguments** on the wrappers: `Db.get_user conn ~id`. Names come
straight from the `:id` in the SQL.

**`result` by default, `_exn` variant alongside**: `get_user` returns
`(row option, Error.t) result`; `get_user_exn` raises `Sqlml.Sql_error`. Follows
the usual OCaml convention, and keeps the unsuffixed name total.

**Real types**: `uuid → Uuidm.t`, `timestamptz → Ptime.t`, `numeric →
Decimal.t`. Postgres prints timestamps as `2026-07-28 09:00:00+00` — a space
instead of RFC3339's `T`, and a 2-digit offset — so `Row.ptime` normalises
before handing to `Ptime.of_rfc3339`.

**Shared model types.** When a result's columns are exactly one table's columns
— every column carries the same non-zero `tableoid` from `RowDescription`, and
together they cover the table — emit one shared `<table>_row` and reuse it
across every such query. Otherwise the query keeps its own type. Without this,
two `SELECT *` queries produce field-for-field identical but *nominally
distinct* records, and no helper can be shared between them. See
`example/wide.mli`.

**Nullable parameters are optional arguments**, sorted after the mandatory ones
so the mandatory arguments stay in SQL order. A query with any nullable
parameter ends in `()`. Trades a trailing unit for dropping a column of
explicit `None`s on wide inserts.

Two mli details the generator must get right, both found by compiling:

1. `include Sqlml.Query.ONE with type row := t` is a *destructive* substitution
   and deletes `type row` from the signature, so the module stops matching
   `ONE`. It must be `type row = t`. (`params :=` is correct, because `params`
   is declared just above the include.)
2. `params` and `row` routinely share field names, and OCaml resolves an
   unannotated `{ id }` to the *last*-defined type — so every generated
   `encode` needs `({ id } : params)` and every `decode` needs `: row`.

## Keeping generated code honest

Generated code keeps compiling after the schema changes under it — a dropped
column, a widened type, a new enum label, a column that becomes NOT NULL — and
only fails at runtime. Every type guarantee here is really "as of the last
`sqlml generate`". `sqlml check` is what converts that into "as of now":

```bash
sqlml generate -q src/sql -o src/db    # write
sqlml check    -q src/sql -o src/db    # verify, exit 1 on any difference
```

`check` regenerates in memory through the *same* pipeline as `generate` — if it
took a different route it could disagree for reasons unrelated to drift — and
reports the first differing line of each file:

```
sqlml: src/db/db.mli:14: out of date
  on disk:   ; display_name : string option
  should be: ; display_name : string
```

Run it in CI against a database migrated to the current schema. Wiring it into
a build:

```
(rule
 (alias runtest)
 (deps (glob_files sql/*.sql) db.ml db.mli)
 (action (run sqlml check -q sql -o . -m db)))
```

## Transactions, and a limitation of modular explicits

```ocaml
val transaction : conn -> (conn -> ('a, Error.t) result) -> ('a, Error.t) result
```

Commits on `Ok`, rolls back on `Error` or on an exception (which is re-raised).
The handle passed to the body has the same type as the outer one, so every
generated query function works inside a transaction unchanged.

It was meant to be a *distinct* type — `tx conn`, phantom tagged, so that a
nested transaction was a type error and an outer handle could not be used inside
the body. **That is not expressible.** Two findings, in order of how much they
constrain the design:

1. **A binding whose type contains a modular-explicit arrow cannot carry an
   explicit polymorphic annotation.** Both

   ```ocaml
   let f : 'k. {Q : S} -> 'k conn -> ... = ...
   let f : type k. {Q : S} -> k conn -> ... = ...
   ```

   are rejected with *"the universal variable would escape its scope"*. So when
   inference declines to generalise a type variable in such a function, there is
   no way to force it.

2. In this codebase the phantom tag did not generalise through `fetch_one` /
   `fetch_all` / `exec`, and by (1) it could not be annotated into submission.
   Minimal reproductions of the shape — phantom parameter, GADT connection,
   modular-explicit binder, module-typed params, `try`/`with` — all generalise
   fine, so the trigger is some narrower interaction that was not worth more
   time to isolate.

Nothing is unsafe today: with a single connection the outer handle *is* the same
connection, so a statement issued through it is still inside the transaction.
The exposure appears only once pooling exists, and the fix there needs no
phantom types — make the pool a distinct type carrying no query operations, so
that obtaining a connection at all requires going through `transaction` or
`with_connection`.

This is worth remembering as a general constraint: modular explicits do not
compose with explicit polymorphic annotations, so any type variable that has to
be universally quantified alongside a `{M : S}` binder is at the mercy of
inference.

## Two drivers

**libpq** (`lib/driver_pg`) — a single connection, no pooling. Fine for CLI
tools, scripts and tests. Returns real affected-row counts via `PQcmdTuples`.

**Caqti over Eio** (`lib/driver_caqti`) — connection pooling and concurrency.
This is what a web app uses.

An earlier revision of this document claimed Caqti could not work because it
sends parameters with explicit type OIDs, so `WHERE uuid_col = $1` would fail
with `operator does not exist: uuid = text`. **That was wrong.** Caqti's
Postgres driver sends parameters with unspecified OIDs and lets the server
infer, exactly as `PQexecParams` with `paramTypes = NULL` does. A `uuid`,
`numeric` or enum parameter round-trips as text with no cast anywhere.

The real obstacle was smaller: Caqti's codecs are static while sqlml's model is
dynamic. Nesting `t2` existentially bridges them in about forty lines, and
nesting rather than `tup3`/`tup4` means there is no arity ceiling. The one thing
the driver needed from above was the column count, since Caqti must declare the
row shape before executing where libpq discovers it from the result — hence
`Query.ONE`/`MANY` exposing `columns`, which the generator knows anyway.

**Eio is direct style, so no generated signature changed.** A query returns a
plain `result`, not a promise. Choosing Lwt instead would have monadified every
generated function.

`Pool.t` deliberately carries no query operations: the only way to reach a
`Sqlml.conn` is `Pool.use` or `Pool.transaction`, both scoped. That is the
enforcement the phantom-typed transaction handle could not provide, achieved by
not exposing the operation rather than by type-level machinery.

The generator still uses libpq directly, and always will — `PQftable` /
`PQftablecol` are the only source of nullability and shared-model detection, and
Caqti cannot expose them. Codegen and runtime are different programs with
different needs.

## Open decisions

- **Named vs positional params.** sqlc uses `:id`; Postgres wants `$1`. The
  parser will rewrite `:name` → `$n` and use the names for the `params` record
  fields. Not yet implemented.
- **Nullability.** Parse/Describe gives types but not nullability. sqlc infers
  it from the schema; Squirrel uses `EXPLAIN` + `pg_attribute` plus `!`/`?`
  column suffixes. Undecided — likely start with `?`/`!` overrides.
- **Eio.** `caqti-eio` is still labelled experimental upstream. The runtime is
  synchronous today and has no IO opinion; direct-style Eio would enter only in
  the driver.
- **SQLite.** `Driver.placeholder` exists to abstract `$1` vs `?`, but SQLite
  has no Parse/Describe equivalent, so type inference there needs a different
  strategy than Postgres.

## Arrays

`text[]`, `int[]`, `uuid[]` and arrays of enums map to OCaml lists. Detection is
from `pg_type`: an array has `typcategory = 'A'` and a `typelem` pointing at its
element type, and for an array of an enum the labels live on the element.

Arrays also give you dynamic IN lists, which is the common reason to want them:

```sql
-- name: GetUsersByIds :many
SELECT id, email FROM users WHERE id = ANY(:ids);
```
```ocaml
val get_users_by_ids : Sqlml.conn -> ids:Uuidm.t list -> (get_users_by_ids_row list, _) result
```

Values cross as Postgres's `{a,b,c}` literal, so `Row.list` and `Value.of_list`
implement its quoting rules: an element is quoted when it is empty, contains a
delimiter, brace, quote, backslash or whitespace, or would otherwise read back
as the literal NULL. The round-trip is tested against all of those.

Two deliberate limits. **Nested arrays are rejected** rather than flattened.
**A NULL element is an error**, because Postgres does not report whether array
elements are nullable — inventing an `option` there would be guessing, and
silently dropping it would be worse.
