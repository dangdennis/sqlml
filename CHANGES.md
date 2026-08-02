# Changes

## 0.1.0 (unreleased)

First release. `.sql` files in, typed OCaml out, with every query's parameter
and result types asked from a live PostgreSQL at codegen; the execution API is
built on modular explicits (OCaml ≥ 5.5.0), so the query module passed
determines the parameter and result types.

- Three packages: `sqlml` (runtime; no database client of its own),
  `sqlml-postgresql` (the `sqlml` CLI — generate, check, describe — and a
  libpq driver with a prepared-statement cache), `sqlml-caqti` (Caqti/Eio
  driver with connection pooling).
- Cardinalities `:one`, `:one!`, `:many`, `:exec`, checked at compile time via
  witness types; `fetch_fold` streams `:many` results through a server-side
  cursor.
- Nullability from `pg_attribute.attnotnull`, overridable per column with
  `!`/`?` alias markers; nullable parameters with `:name?`.
- Shared model row types when a query returns exactly one table's full column
  set; `sqlml.toml` renames and custom OCaml types per column or per Postgres
  type.
- Optional blocks (`/*? ... */`) for dynamic filters: every inclusion
  combination is rewritten and verified against the database at codegen.
- Type mappings for uuid, numeric (decimal), timestamptz/timestamp, date,
  time, interval (all four IntervalStyles parsed), json/jsonb, enums, arrays
  (including arrays of enums), bytea; unmapped types are a hard error.
- Transactions with savepoint nesting, isolation levels, and opt-in retry on
  serialization failure/deadlock; server errors carry their SQLSTATE and
  diagnostic fields.
- `sqlml check` byte-compares regenerated output for CI; generated code is
  formatted with the pinned ocamlformat.
- `sqlml snapshot` caches the database's answers in a committed
  `sqlml.snapshot.json`, and `generate`/`check` accept `--offline`: no
  database needed, with loud staleness errors keyed on every SQL variant's
  hash and on the schema files listed in `sqlml.toml`.
