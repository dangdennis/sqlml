(* sqlml CLI. Currently only `describe`, which dumps what Postgres told us about
   each query -- the input the emitter will consume. *)

let die fmt = Printf.ksprintf (fun s -> prerr_endline s; exit 1) fmt

let describe_files files =
  let queries =
    List.concat_map
      (fun f ->
        match Sqlml_gen.Parse.of_file f with
        | Ok qs -> qs
        | Error (e : Sqlml_gen.Parse.error) ->
          die "%s:%d: %s" e.Sqlml_gen.Parse.file e.Sqlml_gen.Parse.line e.Sqlml_gen.Parse.message)
      files
  in
  let conninfo = Sqlml_gen.Describe.conninfo_of_env () in
  let conn =
    match Sqlml_gen.Describe.connect conninfo with
    | Ok c -> c
    | Error m -> die "could not connect: %s" m
  in
  (match Sqlml_gen.Describe.describe_all conn queries with
   | Error e -> die "%s" (Sqlml_gen.Describe.string_of_error e)
   | Ok described ->
     List.iter
       (fun (d : Sqlml_gen.Describe.described) ->
         let q = d.Sqlml_gen.Describe.query in
         Printf.printf "%s  (:%s)  %s:%d\n" q.Sqlml_gen.Parse.name
           (Sqlml_gen.Parse.string_of_cardinality q.Sqlml_gen.Parse.cardinality)
           q.Sqlml_gen.Parse.file q.Sqlml_gen.Parse.line;
         (match d.Sqlml_gen.Describe.model_table with
          | Some t -> Printf.printf "  model    : %s (full row)\n" t
          | None -> ());
         List.iter
           (fun (p : Sqlml_gen.Describe.param) ->
             Printf.printf "  param $%d : %-14s %s%s\n" p.Sqlml_gen.Describe.index
               p.Sqlml_gen.Describe.ptype_name p.Sqlml_gen.Describe.pname
               (match p.Sqlml_gen.Describe.penum_labels with
                | [] -> ""
                | l -> "  enum{" ^ String.concat "|" l ^ "}"))
           d.Sqlml_gen.Describe.params;
         List.iter
           (fun (c : Sqlml_gen.Describe.column) ->
             Printf.printf "  col      : %-14s %-14s %s%s%s\n" c.Sqlml_gen.Describe.name
               c.Sqlml_gen.Describe.type_name
               (if c.Sqlml_gen.Describe.nullable then "nullable" else "NOT NULL")
               (if c.Sqlml_gen.Describe.table_oid = 0 then "  [computed]"
                else Printf.sprintf "  [tbl %d col %d]" c.Sqlml_gen.Describe.table_oid
                       c.Sqlml_gen.Describe.table_col)
               (match c.Sqlml_gen.Describe.enum_labels with
                | [] -> ""
                | l -> "  enum{" ^ String.concat "|" l ^ "}"))
           d.Sqlml_gen.Describe.columns;
         print_newline ())
       described);
  Sqlml_gen.Pq.finish conn

let () =
  match Array.to_list Sys.argv with
  | _ :: "describe" :: (_ :: _ as files) -> describe_files files
  | _ :: "describe" :: [] -> die "usage: sqlml describe FILE.sql..."
  | _ -> die "usage: sqlml describe FILE.sql..."
