# AGENTS.md

Instructions for AI agents working on sqlml. Humans welcome too.

## What this is

sqlc for OCaml: `.sql` files in, typed OCaml out, with types asked from a live
PostgreSQL at codegen (libpq Parse/Describe + catalog). The execution API uses
modular explicits (OCaml >= 5.5.0): the query module passed determines the
parameter and result types.

DESIGN.md records why things are the way they are. ROADMAP.md records what is
done and deferred. Read the relevant section before changing a decision.

## Environment

```
docker compose up -d           # PostgreSQL 18 on 127.0.0.1:55432
export DATABASE_URL=postgresql://sqlml:sqlml@127.0.0.1:55432/sqlml
```

Dependencies are managed by dune package management: `dune.lock/` is
committed, and a plain `dune build` fetches and builds everything including
the compiler (OCaml 5.5.0 — nothing older builds this). The first build is
slow; afterwards it is cached. `dune pkg lock` regenerates the lock after a
dependency change in `dune-project`. An opam switch (`ocaml55`) works too and
is what CI currently uses.

The generator shells out to `ocamlformat`; make sure one matching
`.ocamlformat`'s pinned version is on PATH or generated output will not match
`sqlml check`.

Schema is `example/schema.sql`, applied on first container boot. After editing
it: `docker compose down -v && docker compose up -d`.

## Commands

```
dune build                     # everything
dune test                      # unit tests; no database needed
dune exec example/e2e.exe      # end-to-end against Postgres (libpq driver)
dune exec example/web.exe      # pooled Caqti driver, concurrency, streaming
dune exec bin/main.exe -- generate -q example/sql -o example/generated
dune exec bin/main.exe -- check -q example/sql -o example/generated
dune build @fmt                # must be clean; version-pinned ocamlformat
dune build @doc
dune build -p sqlml,sqlml-postgresql,sqlml-caqti @install   # release isolation
```

All of the above must pass before a commit. CI runs the same set.

## Layout

| Path | Role |
|---|---|
| `lib/sqlml/` | Runtime. Generated code depends on this and nothing else. |
| `lib/pq/` | Raw libpq binding (C stubs), shared by generator and driver. |
| `lib/driver_pg/` | libpq driver: statement cache, full error diagnostics. |
| `lib/driver_caqti/` | Caqti/Eio driver: pooling. |
| `lib/generator/` | parse → describe → typemap → resolve → render pipeline; config. |
| `bin/` | `sqlml` CLI (cmdliner). |
| `example/` | Schema, queries, generated output, runnable programs, e2e. |
| `test/` | Unit tests. |

## Invariants — do not break these

- `lib/sqlml` never depends on Caqti, Eio, or libpq. The driver boundary is
  `Sqlml.Driver.S`; backends live below it, generated signatures above it.
- `example/generated/` is generator output. Never hand-edit; change the
  generator or the SQL and regenerate. `sqlml check` compares byte-for-byte
  and must exit 0.
- `generate` and `check` share one pipeline (`Run.build`), including the
  ocamlformat pass over emitted code. Do not fork them.
- Every SQL text the runtime can execute must have been verified by Describe
  at codegen. For dynamic queries that means every variant, not just the full
  one.
- Decoders accept what PostgreSQL prints; encoders print what PostgreSQL
  accepts regardless of session style. Drivers pin `datestyle = ISO`;
  `IntervalStyle` is handled by parsing all four styles instead.
- Unmapped Postgres types are a hard error naming the column — never a silent
  fallback to string.
- Server failures carry a SQLSTATE through `Driver.error`; `_exn` functions
  are derived from the `result` ones via `or_raise`, never implemented
  separately.

## Adding a type mapping

Touch all of: `Typemap` (variant + ocaml_type/decoder/encoder/elem cases),
`Row` (+ `Row.Elem`), `Value` (+ `Value.Print`), the README type table, and an
e2e round-trip through a real column. Grep for how `date` was added.

## Tests

- `dune test` must not require a database or network.
- Anything that crosses the wire — new type, new SQL feature, driver change —
  gets an e2e assertion against real data, in `example/e2e.ml`.
- Behavioural over mocked: e2e provokes real errors (serialization failures
  via racing connections, prepared statements killed by rollback, hostile
  DateStyle) rather than constructing error values.
- When a patch script edits source, assert the old text was found; a silent
  no-op replace once shipped a missing fix.

## Style

- ocamlformat is pinned in `.ocamlformat`; run `dune build @fmt` and promote.
- Comments explain constraints and why — never what the next line does. The
  long-form rationale lives in commit messages and DESIGN.md.
- Errors are values; name the file, line, and query in generator errors.
- Commit messages: imperative summary line, then the reasoning. This repo's
  history is its second documentation; keep it that way.
