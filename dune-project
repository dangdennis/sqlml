(lang dune 3.16)
(name sqlml)
(generate_opam_files true)

(source (github dangggdennis/sqlml))
(authors "Dennis Dang")
(maintainers "Dennis Dang")
(license MIT)
(documentation "https://dangggdennis.github.io/sqlml")

; Three packages, so that depending on the runtime does not drag in libpq,
; Caqti and Eio. Generated code needs only `sqlml`.

(package
 (name sqlml)
 (synopsis "Typed OCaml from your SQL, checked against PostgreSQL")
 (description
  "Write .sql files with sqlc-style annotations; sqlml asks PostgreSQL for the\
 \n parameter and result types of each query and emits typed OCaml. This package\
 \n is the runtime that generated code depends on: value encoding and decoding,\
 \n the query interface, transactions, and the driver boundary. It pulls in no\
 \n database client of its own -- add sqlml-postgresql or sqlml-caqti for that.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.16))
  (ptime (>= 1.0.0))
  (uuidm (>= 0.9.8))
  (decimal (>= 1.0.0))
  (yojson (>= 2.0.0))))

(package
 (name sqlml-postgresql)
 (synopsis "The sqlml code generator, CLI, and libpq driver")
 (description
  "Provides the `sqlml` command (generate, check, describe) and a PostgreSQL\
 \n driver built directly on libpq. The generator needs libpq rather than a\
 \n higher-level client because it uses PQdescribePrepared together with\
 \n PQftable and PQftablecol, which are what tell it a result column's\
 \n originating table -- the basis for nullability and shared row types.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.16))
  (sqlml (= :version))
  (cmdliner (>= 1.3.0))
  conf-postgresql))

(package
 (name sqlml-caqti)
 (synopsis "Caqti driver for sqlml, with connection pooling")
 (description
  "Runs generated queries through Caqti over Eio, which adds connection\
 \n pooling and concurrency. Eio is direct style, so generated signatures are\
 \n unchanged: a query returns a plain result rather than a promise.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.16))
  (sqlml (= :version))
  (caqti (>= 3.0.0))
  (caqti-eio (>= 3.0.0))
  (uri (>= 4.0.0))))
