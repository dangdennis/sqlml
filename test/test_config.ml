(* sqlml.toml parsing: renames, custom-type lookup precedence, and the error
   paths. Uses a temp directory since Config reads a file. *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

open Sqlml_gen

let with_toml contents f =
  let dir = Filename.temp_file "sqlml_cfg" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let oc = open_out (Filename.concat dir "sqlml.toml") in
  output_string oc contents;
  close_out oc;
  Fun.protect
    ~finally:(fun () ->
      Sys.remove (Filename.concat dir "sqlml.toml");
      Unix.rmdir dir)
    (fun () -> f (Config.load dir))

let () =
  (* missing file is empty config *)
  let dir = Filename.temp_file "sqlml_nocfg" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  check "missing file is Ok empty"
    (match Config.load dir with
    | Ok c -> Config.renamed c "users" = None
    | Error _ -> false);
  Unix.rmdir dir;

  with_toml
    {|
[rename]
users = "user"
"users.display_name" = "name"

[types."users.id"]
ocaml = "User_id.t"
of_string = "User_id.of_string"
to_string = "User_id.to_string"

[types.citext]
ocaml = "Email.t"
of_string = "Email.of_string"
to_string = "Email.to_string"
|}
    (function
    | Error d -> check ("config parses: " ^ Diag.to_string d) false
    | Ok c ->
        check "table rename" (Config.renamed c "users" = Some "user");
        check "field rename" (Config.renamed c "users.display_name" = Some "name");
        check "unknown rename is None" (Config.renamed c "posts" = None);
        (* precedence: exact key beats the bare pg type *)
        check "per-column custom"
          (match Config.custom c ~key:(Some "users.id") ~pg_type:"uuid" with
          | Some { Config.ocaml; _ } -> ocaml = "User_id.t"
          | None -> false);
        (* the per-type fallback: a column with no exact key, matched by its
           postgres type name alone -- proving this branch is reachable *)
        check "per-type fallback reachable"
          (match Config.custom c ~key:(Some "users.email") ~pg_type:"citext" with
          | Some { Config.ocaml; _ } -> ocaml = "Email.t"
          | None -> false);
        check "no key, per-type still applies"
          (match Config.custom c ~key:None ~pg_type:"citext" with
          | Some _ -> true
          | None -> false);
        check "neither matches -> None"
          (Config.custom c ~key:(Some "users.email") ~pg_type:"text" = None));

  (* error paths name the problem *)
  with_toml "[types.citext]\nocaml = \"Email.t\"\n" (function
    | Error d ->
        let m = Diag.to_string d in
        let contains hay needle =
          let nh = String.length hay and nn = String.length needle in
          let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
          go 0
        in
        check "missing of_string reported" (contains m "of_string")
    | Ok _ -> check "missing of_string reported" false);

  with_toml "not toml at all [[[" (function
    | Error _ -> check "syntax error reported" true
    | Ok _ -> check "syntax error reported" false);

  print_endline "all good"
