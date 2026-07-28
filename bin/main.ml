(* sqlml CLI.

     sqlml describe FILE.sql...          dump what Postgres says about each query
     sqlml generate --queries DIR --out DIR [--module NAME]

   Both need a live Postgres via DATABASE_URL (or PGHOST/PGPORT/PGUSER/
   PGDATABASE/PGPASSWORD). *)

open Sqlml_gen

let die fmt = Printf.ksprintf (fun s -> prerr_endline s; exit 1) fmt

let sql_files dir =
  match Sys.readdir dir with
  | exception Sys_error m -> die "%s" m
  | entries ->
    entries |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".sql")
    |> List.sort compare
    |> List.map (Filename.concat dir)

let parse_all files =
  List.concat_map
    (fun f ->
      match Parse.of_file f with
      | Ok qs -> qs
      | Error (e : Parse.error) -> die "%s:%d: %s" e.Parse.file e.Parse.line e.Parse.message)
    files

let connect () =
  match Describe.connect (Describe.conninfo_of_env ()) with
  | Ok c -> c
  | Error m -> die "could not connect: %s" m

let describe_files files =
  let queries = parse_all files in
  let conn = connect () in
  (match Describe.describe_all conn queries with
   | Error e -> die "%s" (Describe.string_of_error e)
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
             Printf.printf "  param $%d : %-14s %s%s%s\n" p.Describe.index p.Describe.ptype_name
               p.Describe.pname
               (if p.Describe.pnullable then "  [nullable]" else "")
               (match p.Describe.penum_labels with
                | [] -> ""
                | l -> "  enum{" ^ String.concat "|" l ^ "}"))
           d.Describe.params;
         List.iter
           (fun (c : Describe.column) ->
             Printf.printf "  col      : %-14s %-14s %s%s%s\n" c.Describe.name c.Describe.type_name
               (if c.Describe.nullable then "nullable" else "NOT NULL")
               (if c.Describe.table_oid = 0 then "  [computed]"
                else Printf.sprintf "  [tbl %d col %d]" c.Describe.table_oid c.Describe.table_col)
               (match c.Describe.enum_labels with
                | [] -> ""
                | l -> "  enum{" ^ String.concat "|" l ^ "}"))
           d.Describe.columns;
         print_newline ())
       described);
  Pq.finish conn

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let generate ~queries_dir ~out_dir ~module_name =
  let files = sql_files queries_dir in
  if files = [] then die "no .sql files under %s" queries_dir;
  let queries = parse_all files in
  let conn = connect () in
  let described =
    match Describe.describe_all conn queries with
    | Error e -> die "%s" (Describe.string_of_error e)
    | Ok d -> d
  in
  Pq.finish conn;
  match Emit.generate ~src:queries_dir described with
  | Error m -> die "%s" m
  | Ok (mli, ml) ->
    if not (Sys.file_exists out_dir) then Unix.mkdir out_dir 0o755;
    let base = Filename.concat out_dir module_name in
    write_file (base ^ ".mli") mli;
    write_file (base ^ ".ml") ml;
    Printf.printf "wrote %s.mli and %s.ml (%d queries from %d file(s))\n" base base
      (List.length described) (List.length files)

let () =
  let rec opts acc = function
    | [] -> acc
    | k :: v :: tl when String.length k > 2 && String.sub k 0 2 = "--" ->
      opts ((String.sub k 2 (String.length k - 2), v) :: acc) tl
    | k :: _ -> die "unexpected argument %S" k
  in
  match Array.to_list Sys.argv with
  | _ :: "describe" :: (_ :: _ as files) -> describe_files files
  | _ :: "generate" :: rest ->
    let o = opts [] rest in
    let get k d =
      match List.assoc_opt k o with Some v -> v | None -> (match d with Some d -> d | None -> die "generate: --%s is required" k)
    in
    generate ~queries_dir:(get "queries" None) ~out_dir:(get "out" None)
      ~module_name:(get "module" (Some "db"))
  | _ ->
    prerr_endline "usage:";
    prerr_endline "  sqlml describe FILE.sql...";
    prerr_endline "  sqlml generate --queries DIR --out DIR [--module NAME]";
    exit 1
