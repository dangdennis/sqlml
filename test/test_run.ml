(* Run's pure logic: discovery, uniqueness, and the drift diff. *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

open Sqlml_gen

let mkdir d = try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let write path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

let () =
  (* ---------- discovery ---------- *)
  let root = Filename.temp_file "sqlml_disc" "" in
  Sys.remove root;
  mkdir root;
  mkdir (Filename.concat root "sub");
  mkdir (Filename.concat root ".hidden");
  mkdir (Filename.concat root "_build");
  write (Filename.concat root "b.sql") "";
  write (Filename.concat root "a.sql") "";
  write (Filename.concat root "sub/c.sql") "";
  write (Filename.concat root ".hidden/x.sql") "";
  write (Filename.concat root "_build/y.sql") "";
  write (Filename.concat root "notes.txt") "";
  (match Run.sql_files root with
  | Ok fs ->
      let rel = List.map (fun f -> Filename.basename f) fs in
      check "recursive, sorted, filtered" (rel = [ "a.sql"; "b.sql"; "c.sql" ])
  | Error _ -> check "recursive, sorted, filtered" false);
  check "missing dir is a diag"
    (match Run.sql_files (Filename.concat root "nope") with
    | Error _ -> true
    | Ok _ -> false);

  (* ---------- uniqueness is on the snake_cased name ---------- *)
  let q name =
    Result.get_ok (Parse.of_string ~file:"t.sql" ("-- name: " ^ name ^ " :one\nSELECT 1"))
  in
  check "distinct names pass" (Run.check_unique_names (q "GetUser" @ q "GetPost") = Ok ());
  check "same name collides"
    (match Run.check_unique_names (q "GetUser" @ q "GetUser") with
    | Error _ -> true
    | Ok _ -> false);
  check "GetUser and Get_user collide"
    (match Run.check_unique_names (q "GetUser" @ q "Get_user") with
    | Error _ -> true
    | Ok _ -> false);

  (* ---------- drift diff ---------- *)
  let d = Run.first_difference ~path:"p.ml" ~expected:"a\nb\nc" ~actual:"a\nX\nc" in
  (match d with
  | Some (Run.Differs { line; expected; actual; _ }) ->
      check "first differing line, 1-based" (line = 2 && expected = "b" && actual = "X")
  | _ -> check "first differing line, 1-based" false);
  check "identical is None"
    (Run.first_difference ~path:"p" ~expected:"same" ~actual:"same" = None);
  (match Run.first_difference ~path:"p" ~expected:"a\nb" ~actual:"a" with
  | Some (Run.Differs { line = 2; actual = "(end of file)"; _ }) ->
      check "expected longer" true
  | _ -> check "expected longer" false);
  (match Run.first_difference ~path:"p" ~expected:"a" ~actual:"a\nb" with
  | Some (Run.Differs { line = 2; expected = "(end of file)"; _ }) ->
      check "actual longer" true
  | _ -> check "actual longer" false);

  print_endline "all good"
