(lang dune 3.16)
(name sqlml)
(generate_opam_files true)

(source (github dangggdennis/sqlml))
(authors "Dennis Dang")
(maintainers "Dennis Dang")
(license MIT)

(package
 (name sqlml)
 (synopsis "SQL-first, type-safe query compiler for OCaml")
 (description
  "sqlml compiles .sql files into typed OCaml query modules, in the style of\
 \n sqlc. Types are inferred by asking PostgreSQL directly (Parse/Describe),\
 \n and the generated modules are executed through a small runtime whose\
 \n execution API uses modular explicits: the query module you pass determines\
 \n the parameter and result types.")
 (depends
  (ocaml (>= 5.3.0))
  (dune (>= 3.16))))
