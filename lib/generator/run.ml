(* See run.mli for why generate and check share one pipeline. *)

type built = { mli : string; ml : string; queries : int; files : int }

type drift =
  | Missing of string (* path *)
  | Differs of { path : string; line : int; expected : string; actual : string }

(* Every .sql file under [dir], recursively. Sorted so generated output is
   deterministic regardless of readdir order. Hidden directories and _build are
   skipped. *)
let sql_files dir =
  if not (Sys.file_exists dir) then Diag.error "no such directory: %s" dir
  else if not (Sys.is_directory dir) then Diag.error "not a directory: %s" dir
  else begin
    let acc = ref [] in
    let rec walk d =
      match Sys.readdir d with
      | exception Sys_error _ -> ()
      | entries ->
          Array.sort compare entries;
          Array.iter
            (fun e ->
              let path = Filename.concat d e in
              if String.length e > 0 && e.[0] = '.' then ()
              else if e = "_build" then ()
              else if Sys.is_directory path then walk path
              else if Filename.check_suffix e ".sql" then acc := path :: !acc)
            entries
    in
    walk dir;
    let fs = List.sort compare !acc in
    if fs = [] then Diag.error "no .sql files under %s" dir else Ok fs
  end

let parse_all files =
  let rec go acc = function
    | [] -> Ok (List.concat (List.rev acc))
    | f :: tl -> (
        match Parse.of_file f with Ok qs -> go (qs :: acc) tl | Error e -> Error e)
  in
  go [] files

open Gen_util

(* All queries land in one module regardless of which file they came from, so a
   name used twice would emit two modules of the same name and produce output
   that does not compile. Caught here, before the database round-trip, with both
   locations named. *)
let check_unique_names (queries : Parse.t list) =
  let seen = Hashtbl.create 16 in
  let rec go = function
    | [] -> Ok ()
    | (q : Parse.t) :: tl -> (
        (* keyed on the snake_cased form: GetUser and Get_user are distinct
           query names but generate the same get_user function *)
        match Hashtbl.find_opt seen (Parse.to_snake q.Parse.name) with
        | Some (f, l) ->
            Diag.error ~file:q.Parse.file ~line:q.Parse.line
              "query name %S collides with one already defined at %s:%d (names that \
               differ only in casing generate the same OCaml identifiers)\n\
              \  query names must be unique across every .sql file, because they all \
               generate into one module"
              q.Parse.name f l
        | None ->
            Hashtbl.replace seen (Parse.to_snake q.Parse.name) (q.Parse.file, q.Parse.line);
            go tl)
  in
  go queries

type source = Live of string | Offline

let describe_live ~conninfo queries =
  let* conn = Describe.connect conninfo in
  let described = Describe.describe_all conn queries in
  Pq.finish conn;
  described

let build ~queries_dir ~source =
  let* files = sql_files queries_dir in
  let* queries = parse_all files in
  let* () = check_unique_names queries in
  (* config is read before the database round-trip: a typo in sqlml.toml
     should not require a live server to be reported *)
  let* config = Config.load queries_dir in
  let* described =
    match source with
    | Live conninfo -> describe_live ~conninfo queries
    | Offline -> Snapshot.describe_offline ~queries_dir ~config queries
  in
  let* mli, ml = Emit.generate ~config ~src:queries_dir described in
  Ok { mli; ml; queries = List.length described; files = List.length files }

let snapshot ~queries_dir ~conninfo =
  let* files = sql_files queries_dir in
  let* queries = parse_all files in
  let* () = check_unique_names queries in
  let* config = Config.load queries_dir in
  let* described = describe_live ~conninfo queries in
  let* path = Snapshot.write ~queries_dir ~config described in
  Ok (path, List.length described)

(* Run generated source through ocamlformat, using whatever .ocamlformat applies
   to the output directory, so the result is already in the project's own style.

   This matters because `check` compares byte-for-byte. Without it, a user whose
   editor formats on save would see check fail forever on code they did not
   write. Formatting here means generate and check agree by construction --
   which is also why a missing or mismatched ocamlformat is a loud failure
   rather than a skipped nicety: the two commands must format identically. *)
let format ~name contents =
  let tmp = Filename.temp_file "sqlml_fmt" (Filename.extension name) in
  let cleanup () = try Sys.remove tmp with _ -> () in
  Fun.protect ~finally:cleanup (fun () ->
      let oc = open_out_bin tmp in
      output_string oc contents;
      close_out oc;
      (* --name resolves .ocamlformat from the real output path while the bytes
         live in the system temp dir, never in the user's source tree *)
      let rc =
        Sys.command
          (Printf.sprintf "ocamlformat --inplace --name %s %s 2>/dev/null"
             (Filename.quote name) (Filename.quote tmp))
      in
      match rc with
      | 0 ->
          let ic = open_in_bin tmp in
          let n = in_channel_length ic in
          let out = really_input_string ic n in
          close_in ic;
          Ok out
      | 127 ->
          Diag.error
            "ocamlformat not found on PATH. generate and check format their output \
             through the project's pinned ocamlformat (see .ocamlformat); without it the \
             two could disagree byte-for-byte."
      | rc ->
          Diag.error
            "ocamlformat failed (exit %d) formatting %s -- is the installed version the \
             one pinned in .ocamlformat?"
            rc name)

let format_built ~out_dir ~module_name b =
  let base = Filename.concat out_dir module_name in
  let* mli = format ~name:(base ^ ".mli") b.mli in
  let* ml = format ~name:(base ^ ".ml") b.ml in
  Ok { b with mli; ml }

(* The describe subcommand's pipeline: everything build does, minus emit. *)
let describe ~queries_dir ~conninfo =
  let* files = sql_files queries_dir in
  let* queries = parse_all files in
  let* () = check_unique_names queries in
  let* conn = Describe.connect conninfo in
  let described = Describe.describe_all conn queries in
  Pq.finish conn;
  described

let paths ~out_dir ~module_name =
  let base = Filename.concat out_dir module_name in
  (base ^ ".mli", base ^ ".ml")

let read_file path =
  if not (Sys.file_exists path) then None
  else
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Some s

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let rec mkdir_p dir =
  if dir <> "" && dir <> "/" && dir <> "." && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write ~out_dir ~module_name b =
  match
    let* b = format_built ~out_dir ~module_name b in
    mkdir_p out_dir;
    let mli_path, ml_path = paths ~out_dir ~module_name in
    write_file mli_path b.mli;
    write_file ml_path b.ml;
    Ok (mli_path, ml_path)
  with
  | r -> r
  | exception Sys_error m -> Diag.error "cannot write output: %s" m
  | exception Unix.Unix_error (e, _, p) ->
      Diag.error "cannot write output: %s: %s" p (Unix.error_message e)

(* First differing line, so the report points at something actionable rather
   than just saying the files differ. *)
let first_difference ~path ~expected ~actual =
  let e = String.split_on_char '\n' expected and a = String.split_on_char '\n' actual in
  let rec go i e a =
    match (e, a) with
    | [], [] -> None
    | eh :: et, ah :: at -> if eh = ah then go (i + 1) et at else Some (i, eh, ah)
    | eh :: _, [] -> Some (i, eh, "(end of file)")
    | [], ah :: _ -> Some (i, "(end of file)", ah)
  in
  match go 1 e a with
  | None -> None
  | Some (line, expected, actual) -> Some (Differs { path; line; expected; actual })

let check ~out_dir ~module_name b =
  (* the same formatting generate applies, so the comparison is like-for-like;
     a formatting failure is an error, not drift -- reporting it as drift sent
     users chasing a schema change that did not exist *)
  let* b = format_built ~out_dir ~module_name b in
  Ok
    (List.filter_map
       (fun (path, expected) ->
         match read_file path with
         | None -> Some (Missing path)
         | Some actual ->
             if actual = expected then None else first_difference ~path ~expected ~actual)
       (let mli_path, ml_path = paths ~out_dir ~module_name in
        [ (mli_path, b.mli); (ml_path, b.ml) ]))

let string_of_drift = function
  | Missing path -> Printf.sprintf "%s: missing -- has `sqlml generate` ever run?" path
  | Differs { path; line; expected; actual } ->
      Printf.sprintf "%s:%d: out of date\n  on disk:   %s\n  should be: %s" path line
        (String.trim actual) (String.trim expected)
