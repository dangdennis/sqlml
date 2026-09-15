# Local validation — 2026-09-15

Toolchain: OCaml 5.5.0, dune 3.24.1, PostgreSQL 18.0, ocamlformat 0.29.0,
pGenie 0.15.0 (verified release executable).

| Gate | Result |
| --- | --- |
| Locked-dependency `dune build` and `dune test` | Passed |
| Opam dependency workflow build and unit tests | Passed |
| libpq e2e, including nested containers and COPY | Passed |
| Application example and pooled Caqti example | Passed |
| Regenerated code, live check, offline check, snapshot | Passed; 24 example queries |
| Formatting and documentation | Passed |
| Three-package isolated install build | Passed |
| PostgreSQL inference corpus | 1,000 and 10,000 cases passed, seed 0 |
| Compiled generated corpus decoders | Both tiers passed against captured live rows |
| pGenie PR tier | 783 matched; 217 explicitly unsupported |
| pGenie extended tier | 7,814 matched; 2,186 explicitly unsupported |
| Recreated-schema snapshot fingerprint | Identical despite new catalog OIDs |
| Injected differential mismatch and SQL reduction | Failure detected; reduced query verified to retain disagreement |
| Final vector/array-detection refinement | Rechecked with 1,000 cases and compiled decoders |

The extended pGenie exclusions were 1,562 domain cases, 312 custom-range cases,
and 312 dropped-composite-attribute cases. Those cases passed the PostgreSQL and
compiled-decoder gates. No unexplained differences remain in these runs.

The corpus is a documented finite matrix, not a proof for all PostgreSQL programs.
The new CI workflow is configured for PR and weekly/manual tiers; it has not been
run remotely as part of this local work. No release has been published.

Full transient evidence is in `/tmp/sqlml-inference-final`,
`/tmp/sqlml-inference-extended`, and `/tmp/sqlml-inference-final-reviewed` on the
implementation machine. Reproduction commands and limitations are in README.md
in this directory. Migration guidance is in the repository root README.md.
