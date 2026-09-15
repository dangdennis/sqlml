(* The offline snapshot: a round-trip through write/describe_offline must
   reproduce Describe's records exactly, and every staleness path -- missing
   file, missing query, edited SQL, edited schema file -- must fail loudly
   naming the culprit. All offline, via Describe's explicit constructors. *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

open Sqlml_gen

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  go 0

let parse1 src =
  match Parse.of_string ~file:"t.sql" src with Ok [ q ] -> q | _ -> failwith "parse1"

let col ?(table = None) ?(table_oid = 0) ?(table_col = 0) ?(nullable = false)
    ?(labels = []) ?(elem = None) name type_name =
  Describe.v_column ~name ~type_name ~elem_type_name:elem ~table ~table_oid ~table_col
    ~nullable ~enum_labels:labels

let param ?(nullable = false) ?(labels = []) index pname ptype_name =
  Describe.v_param ~index ~pname ~ptype_name ~pelem_type_name:None ~penum_labels:labels
    ~pnullable:nullable

let described ?(model = None) src ~params ~columns =
  Describe.v_described ~query:(parse1 src) ~params ~columns ~model_table:model

let with_dir f =
  let dir = Filename.temp_file "sqlml_snap" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () ->
      Array.iter (fun e -> Sys.remove (Filename.concat dir e)) (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () -> f dir)

let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc contents)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let d_static =
  described "-- name: GetThing :one\nSELECT 1"
    ~params:[ param 1 "id" "uuid" ]
    ~columns:
      [
        col "id" "uuid" ~table:(Some "things") ~table_oid:7 ~table_col:1;
        col "note" "text" ~nullable:true ~table:(Some "things") ~table_oid:7 ~table_col:2;
      ]
    ~model:(Some "things")

let d_dynamic =
  described
    "-- name: FindThings :many\nSELECT a FROM t WHERE o = :o /*? AND e = :e */ LIMIT :n"
    ~params:[ param 1 "o" "uuid"; param 2 "e" "text"; param 3 "n" "int8" ]
    ~columns:[ col "a" "text"; col "s" "status" ~labels:[ "on"; "off" ] ]

let () =
  (* ---------- round-trip: what went in comes back, byte-equal renders ---------- *)
  with_dir (fun dir ->
      (match
         Snapshot.write ~queries_dir:dir ~config:Config.empty [ d_dynamic; d_static ]
       with
      | Error d -> check ("write: " ^ Diag.to_string d) false
      | Ok path ->
          check "write returns the path" (Filename.basename path = Snapshot.filename));
      let first = read_file (Filename.concat dir Snapshot.filename) in
      (match
         Snapshot.describe_offline ~queries_dir:dir ~config:Config.empty
           [ d_static.Describe.query; d_dynamic.Describe.query ]
       with
      | Error d -> check ("offline: " ^ Diag.to_string d) false
      | Ok ds ->
          (* the surrogate table ids differ from live OIDs, so compare what
             matters: the generated source, byte for byte *)
          check "round-trip renders identically"
            (Emit.generate ~src:"s" ds = Emit.generate ~src:"s" [ d_static; d_dynamic ]);
          check "model_table survives"
            (List.exists (fun d -> d.Describe.model_table = Some "things") ds));
      ignore
        (Snapshot.write ~queries_dir:dir ~config:Config.empty [ d_dynamic; d_static ]);
      check "output is deterministic"
        (read_file (Filename.concat dir Snapshot.filename) = first));

  with_dir (fun dir ->
      let path = Filename.concat dir Snapshot.filename in
      write_file path "{\"format_version\":1}";
      match
        Snapshot.describe_offline ~queries_dir:dir ~config:Config.empty [ d_static.query ]
      with
      | Error d ->
          check "v1 snapshot requires regeneration"
            (contains (Diag.to_string d) "re-run `sqlml snapshot`")
      | Ok _ -> check "v1 snapshot requires regeneration" false);

  (* ---------- staleness is loud and names the culprit ---------- *)
  with_dir (fun dir ->
      match Snapshot.describe_offline ~queries_dir:dir ~config:Config.empty [] with
      | Error d ->
          check "missing snapshot names the fix"
            (contains (Diag.to_string d) "sqlml snapshot")
      | Ok _ -> check "missing snapshot names the fix" false);

  with_dir (fun dir ->
      ignore (Snapshot.write ~queries_dir:dir ~config:Config.empty [ d_static ]);
      (match
         Snapshot.describe_offline ~queries_dir:dir ~config:Config.empty
           [ parse1 "-- name: Unknown :exec\nDELETE FROM t" ]
       with
      | Error d -> check "unknown query is named" (contains (Diag.to_string d) "Unknown")
      | Ok _ -> check "unknown query is named" false);
      match
        Snapshot.describe_offline ~queries_dir:dir ~config:Config.empty
          [ parse1 "-- name: GetThing :one\nSELECT 2" ]
      with
      | Error d ->
          let m = Diag.to_string d in
          check "edited SQL is stale" (contains m "GetThing" && contains m "SQL changed")
      | Ok _ -> check "edited SQL is stale" false);

  (* ---------- schema files hash into the snapshot ---------- *)
  with_dir (fun dir ->
      write_file (Filename.concat dir "sqlml.toml") "schema = [\"s.sql\"]\n";
      write_file (Filename.concat dir "s.sql") "CREATE TABLE t (a int);\n";
      let config =
        match Config.load dir with Ok c -> c | Error _ -> failwith "config"
      in
      ignore (Snapshot.write ~queries_dir:dir ~config [ d_static ]);
      check "matching schema passes"
        (Result.is_ok
           (Snapshot.describe_offline ~queries_dir:dir ~config [ d_static.Describe.query ]));
      write_file (Filename.concat dir "s.sql") "CREATE TABLE t (a int, b int);\n";
      (match
         Snapshot.describe_offline ~queries_dir:dir ~config [ d_static.Describe.query ]
       with
      | Error d -> check "edited schema is stale" (contains (Diag.to_string d) "schema")
      | Ok _ -> check "edited schema is stale" false);
      (* the file list itself is part of the contract *)
      match
        Snapshot.describe_offline ~queries_dir:dir ~config:Config.empty
          [ d_static.Describe.query ]
      with
      | Error d ->
          check "changed schema list is stale" (contains (Diag.to_string d) "schema")
      | Ok _ -> check "changed schema list is stale" false);

  print_endline "all good"
