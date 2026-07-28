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
| `test/` — 6 tests over a fake in-memory driver | **passing** |
| `lib/sqlml_caqti` — Caqti/Eio driver | empty, next |
| `lib/generator` — discovery, SQL parse, PG describe, emit | empty |
| `bin/` — `sqlml generate` CLI | empty |

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
