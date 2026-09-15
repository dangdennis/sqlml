# Compiler inference gate

Unit tests remain database- and network-free. This integration suite needs an
**empty, disposable PostgreSQL 18 database**. It creates fixture schemas inside a
transaction and rolls back afterwards; existing schemas with those names cause
an error. pGenie creates and drops its own temporary analysis databases.

```sh
export DATABASE_URL=postgresql://sqlml:sqlml@127.0.0.1:55432/sqlml_inference
dune exec integration/inference.exe -- --cases 1000 --output /tmp/sqlml-inference
python3 integration/compile_corpus.py /tmp/sqlml-inference
python3 integration/install_pgenie.py /tmp/sqlml-pgenie
python3 integration/differential.py /tmp/sqlml-inference --pgn /tmp/sqlml-pgenie/pgn
```

Use the same OCaml toolchain for dune and `ocamlfind`. With the opam workflow,
run commands under `opam exec`; `--pkg=disabled` disables dune package management.
For an alternate build directory, pass it to both dune and `compile_corpus.py`.
The ordinary build must produce `lib/sqlml/sqlml.cma` before compiling the corpus.

## Coverage and assertions

The deterministic matrix crosses 32 expressions with 15 SQL shapes; larger runs
vary case literals and repeat the matrix. `--seed N` rotates type selection;
`--case N --seed S` replays one case. This is broad combinatorial coverage, not an
enumeration of every valid SQL program.

| Dimension | Coverage |
| --- | --- |
| SQL | Inner/left/right/full/lateral/self joins, ordinary/materialized/recursive CTEs, subqueries, EXISTS, UNION ALL, window functions and aggregates |
| Types | Integer limits, numeric/varchar modifiers, enums in two schemas, domains and domain arrays, composites, dropped attributes, multidimensional arrays, ranges/multiranges, JSON and date |
| NULL | Scalar NULL, null extension, strict/non-strict functions, NULL array elements, NULL composite versus all-NULL fields |
| Additional regressions | Dynamic parameter-type drift, anonymous record diagnostics, non-array vector rejection, snapshot/live equivalence |
| Wire tests in `example/e2e.ml` | Nested escaping, bytea, both drivers, domain SQLSTATE, custom ranges, and COPY |

Each case is independently prepared/described, compared with raw catalog reads,
and executed. The runner writes generated `corpus.ml`/`.mli` and decoder checks
containing PostgreSQL's exact returned rows. The compilation stage checks emitted
OCaml types and executes those decoders. Failed decoding fails the gate.

The independent PostgreSQL checks include schema/type identity, recursive kinds,
enum order, domain constraints, array delimiters, composite fields/order, range
relationships, result type modifiers, and parameter/result order. Catalog queries
in the oracle do not call the production catalog discovery implementation.

## Differential comparison

pGenie **0.15.0** release archives and executable hashes are pinned. Its official
binary reports `0` for `--version`, so the executable bytes are checked instead.
Supported installation targets: Linux x64 and macOS arm64.

The JSON analysis model is the normalized input to pGenie's generators. Comparison
checks parameter/result identity and order plus enum/composite definitions.
Nullability precision is recorded separately: sqlml defaults to optional, while
pGenie may infer non-null. Array identity is compared without treating pGenie's
inferred dimension count as a restriction on sqlml's runtime dimensions.

Observed limitations of that release are explicit exclusions **only from pGenie**:

- Domains, including nested domain fields: `Domain types are not supported yet`.
- Custom range types: `Range types are not supported yet`.
- Dropped composite attributes: the dropped field reaches inference as OID 0.

pGenie rejects normalized same-name custom types from different schemas in one
project. Cases are split by schema for comparison; PostgreSQL and sqlml still see
them together. Batches of 50 bound pGenie's connection fan-out. None of these
exclusions remove cases from the PostgreSQL or compiled-decoder gates.

Unknown errors, tool crashes, timeouts, missing cases, unexpected types, and
unexplained differences are failures. Reports account for every input case.

## Evidence, replay, and reduction

Artifacts include schema, seed/case count and versions, per-query SQL/contracts,
generated source, live row decoder checks, snapshot fingerprint, and pGenie model,
logs, comparison counts, unsupported cases, and policy differences.

On a PostgreSQL failure, `failure.txt` identifies the exact replay seed/case.
On a differential mismatch, `failure.json` isolates the query and both contracts.
The reducer tries removing the outer SQL shape, reruns pGenie, and retains a
`minimized-case.json` only if the disagreement persists. Otherwise the original
query remains the reproducer. Promote reproducers into permanent regressions.

CI runs 1,000 cases on PRs and 10,000 on weekly/manual runs, including compiled
decoders and pGenie. It recreates the schema for an additional run and compares
snapshot fingerprints, exercising independence from database-local OIDs. Evidence
is uploaded even on failure. No release is published by these jobs.
