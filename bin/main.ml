(* sqlml CLI. *)

open Cmdliner
open Sqlml_gen

let version = "0.1.0"

(* ---------- shared arguments ---------- *)

let queries =
  let doc = "Directory to search for .sql files, recursively." in
  Arg.(value & opt dir "sql" & info [ "q"; "queries" ] ~docv:"DIR" ~doc)

let out =
  let doc = "Directory to write the generated module into." in
  Arg.(required & opt (some string) None & info [ "o"; "out" ] ~docv:"DIR" ~doc)

let module_name =
  let doc = "Name of the generated module, without extension." in
  Arg.(value & opt string "db" & info [ "m"; "module" ] ~docv:"NAME" ~doc)

let database =
  let doc =
    "Postgres connection string. Defaults to \\$DATABASE_URL, or to \
     PGHOST/PGPORT/PGUSER/PGDATABASE/PGPASSWORD."
  in
  Arg.(value & opt (some string) None & info [ "d"; "database" ] ~docv:"URL" ~doc)

let conninfo_of = function Some u -> u | None -> Describe.conninfo_of_env ()

let die fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("sqlml: " ^ s);
      exit 1)
    fmt

let build_or_die ~queries_dir ~database =
  match Run.build ~queries_dir ~conninfo:(conninfo_of database) with
  | Ok b -> b
  | Error d -> die "%s" (Diag.to_string d)

(* ---------- generate ---------- *)

let generate queries_dir out_dir module_name database =
  let b = build_or_die ~queries_dir ~database in
  let mli_path, ml_path =
    match Run.write ~out_dir ~module_name b with
    | Ok p -> p
    | Error d -> die "%s" (Diag.to_string d)
  in
  Printf.printf "wrote %s and %s (%d queries from %d file(s))\n" mli_path ml_path
    b.Run.queries b.Run.files

let generate_cmd =
  let doc = "Generate typed OCaml from .sql files." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Reads every .sql file in the queries directory, asks PostgreSQL for the \
         parameter and result types of each query, and writes a single OCaml module.";
      `P
        "Requires a live PostgreSQL whose schema matches the queries. The generated code \
         does not need one.";
    ]
  in
  Cmd.v
    (Cmd.info "generate" ~doc ~man)
    Term.(const generate $ queries $ out $ module_name $ database)

(* ---------- check ---------- *)

let check queries_dir out_dir module_name database =
  let b = build_or_die ~queries_dir ~database in
  match Run.check ~out_dir ~module_name b with
  | Error d -> die "%s" (Diag.to_string d)
  | Ok [] ->
      Printf.printf "up to date (%d queries from %d file(s))\n" b.Run.queries b.Run.files;
      exit 0
  | Ok drifts ->
      List.iter (fun d -> prerr_endline ("sqlml: " ^ Run.string_of_drift d)) drifts;
      prerr_endline "sqlml: run `sqlml generate` to update";
      exit 1

let check_cmd =
  let doc = "Verify generated code matches the database. Exits non-zero if not." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Regenerates in memory and compares against what is on disk, reporting the first \
         differing line of each file. Exits 1 on any difference.";
      `P
        "This is what makes the types trustworthy over time. Generated code keeps \
         compiling after the schema changes under it -- a dropped column, a widened \
         type, a new enum label -- and only fails at runtime. Run this in CI against a \
         database migrated to the current schema.";
    ]
  in
  Cmd.v (Cmd.info "check" ~doc ~man)
    Term.(const check $ queries $ out $ module_name $ database)

(* ---------- describe ---------- *)

let describe queries_dir database =
  let conninfo = conninfo_of database in
  let files =
    match Run.sql_files queries_dir with
    | Ok f -> f
    | Error d -> die "%s" (Diag.to_string d)
  in
  let qs =
    match Run.parse_all files with Ok q -> q | Error d -> die "%s" (Diag.to_string d)
  in
  let conn =
    match Describe.connect conninfo with
    | Ok c -> c
    | Error m -> die "could not connect: %s" m
  in
  (match Describe.describe_all conn qs with
  | Error e -> die "%s" (Diag.to_string e)
  | Ok described ->
      List.iter
        (fun (d : Describe.described) ->
          let q = d.Describe.query in
          Printf.printf "%s  (:%s)  %s:%d\n" q.Parse.name
            (Parse.string_of_cardinality q.Parse.cardinality)
            q.Parse.file q.Parse.line;
          (match d.Describe.model_table with
          | Some t -> Printf.printf "  model    : %s (full row)\n" t
          | None -> ());
          List.iter
            (fun (p : Describe.param) ->
              Printf.printf "  param $%d : %-14s %s%s%s\n" p.Describe.index
                p.Describe.ptype_name p.Describe.pname
                (if p.Describe.pnullable then "  [nullable]" else "")
                (match p.Describe.penum_labels with
                | [] -> ""
                | l -> "  enum{" ^ String.concat "|" l ^ "}"))
            d.Describe.params;
          List.iter
            (fun (c : Describe.column) ->
              Printf.printf "  col      : %-14s %-14s %s%s%s\n" c.Describe.name
                c.Describe.type_name
                (if c.Describe.nullable then "nullable" else "NOT NULL")
                (if c.Describe.table_oid = 0 then "  [computed]"
                 else
                   Printf.sprintf "  [tbl %d col %d]" c.Describe.table_oid
                     c.Describe.table_col)
                (match c.Describe.enum_labels with
                | [] -> ""
                | l -> "  enum{" ^ String.concat "|" l ^ "}"))
            d.Describe.columns;
          print_newline ())
        described);
  Pq.finish conn

let describe_cmd =
  let doc = "Print what PostgreSQL says about each query." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Diagnostic. Shows inferred parameter types, result columns with their \
         nullability and originating table, enum labels, and whether the result is \
         exactly one table's row.";
    ]
  in
  Cmd.v (Cmd.info "describe" ~doc ~man) Term.(const describe $ queries $ database)

(* ---------- entry point ---------- *)

let main =
  let doc = "SQL-first, type-safe query compiler for OCaml" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "You write .sql files with sqlc-style annotations; sqlml asks PostgreSQL what \
         the types are and emits typed OCaml.";
      `S Manpage.s_examples;
      `Pre "  sqlml generate -q src/sql -o src/db";
      `Pre "  sqlml check    -q src/sql -o src/db";
      `S Manpage.s_environment;
      `P "DATABASE_URL, or PGHOST / PGPORT / PGUSER / PGDATABASE / PGPASSWORD.";
    ]
  in
  Cmd.group
    (Cmd.info "sqlml" ~version ~doc ~man)
    [ generate_cmd; check_cmd; describe_cmd ]

let () = exit (Cmd.eval main)
