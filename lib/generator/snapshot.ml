(* See snapshot.mli. Hashes are BLAKE256 from the stdlib Digest module -- no
   extra dependency, and MD5 would invite collision arguments nobody needs to
   have. The JSON is sorted and pretty-printed so the committed file diffs
   minimally under review. *)

open Gen_util

let format_version = 1
let filename = "sqlml.snapshot.json"
let hash s = Digest.BLAKE256.to_hex (Digest.BLAKE256.string s)

(* Every SQL text a query can execute, in bitmask order; the snapshot keys on
   all of them so editing any variant -- not just the full one -- invalidates
   the entry. *)
let variant_sqls (q : Parse.t) =
  match q.Parse.dynamic with
  | None -> [ q.Parse.sql ]
  | Some d -> Array.to_list d.Parse.variant_sqls

(* ---------- schema hash ---------- *)

(* One hash over every config-listed schema file, filename-prefixed so content
   cannot slide between file boundaries. Absent list means no staleness check:
   that blind spot is documented, and CI's live job is the backstop. *)
let schema_hash ~queries_dir config =
  match Config.schema config with
  | [] -> Ok None
  | files ->
      let* parts =
        map_result
          (fun f ->
            let path = Filename.concat queries_dir f in
            match In_channel.with_open_bin path In_channel.input_all with
            | contents -> Ok (f ^ "\x00" ^ contents ^ "\x00")
            | exception Sys_error m -> Diag.error "schema file %s: %s" f m)
          files
      in
      Ok (Some (hash (String.concat "" parts)))

(* ---------- serialization ---------- *)

let json_opt = function None -> `Null | Some s -> `String s
let json_strings l = `List (List.map (fun s -> `String s) l)

let json_of_column (c : Describe.column) =
  `Assoc
    [
      ("name", `String c.Describe.name);
      ("type_name", `String c.Describe.type_name);
      ("elem_type_name", json_opt c.Describe.elem_type_name);
      ("table", json_opt c.Describe.table);
      ("table_col", `Int c.Describe.table_col);
      ("nullable", `Bool c.Describe.nullable);
      ("enum_labels", json_strings c.Describe.enum_labels);
    ]

let json_of_param (p : Describe.param) =
  `Assoc
    [
      ("pname", `String p.Describe.pname);
      ("ptype_name", `String p.Describe.ptype_name);
      ("pelem_type_name", json_opt p.Describe.pelem_type_name);
      ("penum_labels", json_strings p.Describe.penum_labels);
      ("pnullable", `Bool p.Describe.pnullable);
    ]

let json_of_described (d : Describe.described) =
  let q = d.Describe.query in
  `Assoc
    [
      ("name", `String q.Parse.name);
      ("variants", json_strings (List.map hash (variant_sqls q)));
      ("params", `List (List.map json_of_param d.Describe.params));
      ("columns", `List (List.map json_of_column d.Describe.columns));
      ("model_table", json_opt d.Describe.model_table);
    ]

let write ~queries_dir ~config described =
  let* schema = schema_hash ~queries_dir config in
  let sorted =
    List.sort
      (fun a b -> compare a.Describe.query.Parse.name b.Describe.query.Parse.name)
      described
  in
  let json =
    `Assoc
      [
        ("format_version", `Int format_version);
        ("schema_files", json_strings (Config.schema config));
        ("schema_hash", json_opt schema);
        ("queries", `List (List.map json_of_described sorted));
      ]
  in
  let path = Filename.concat queries_dir filename in
  match
    Out_channel.with_open_bin path (fun oc ->
        Out_channel.output_string oc (Yojson.Safe.pretty_to_string json);
        Out_channel.output_char oc '\n')
  with
  | () -> Ok path
  | exception Sys_error m -> Diag.error "%s: %s" path m

(* ---------- deserialization ---------- *)

type entry = {
  e_variants : string list;
  e_params : Describe.param list;
  e_columns : Describe.column list;
  e_model : string option;
}

let mem name kvs =
  match List.assoc_opt name kvs with
  | Some v -> Ok v
  | None -> Diag.error "snapshot: missing field %S" name

let as_string = function
  | `String s -> Ok s
  | _ -> Diag.error "snapshot: expected a string"

let as_opt = function
  | `Null -> Ok None
  | `String s -> Ok (Some s)
  | _ -> Diag.error "snapshot: expected a string or null"

let as_strings = function
  | `List l -> map_result as_string l
  | _ -> Diag.error "snapshot: expected a list of strings"

let as_assoc = function
  | `Assoc kvs -> Ok kvs
  | _ -> Diag.error "snapshot: expected an object"

let entry_of_json json =
  let* kvs = as_assoc json in
  let* name = mem "name" kvs in
  let* name = as_string name in
  let* variants = mem "variants" kvs in
  let* e_variants = as_strings variants in
  let* params = mem "params" kvs in
  let* params =
    match params with `List l -> Ok l | _ -> Diag.error "snapshot: params"
  in
  let* e_params =
    map_result
      (fun p ->
        let* kvs = as_assoc p in
        let* pname = Result.bind (mem "pname" kvs) as_string in
        let* ptype_name = Result.bind (mem "ptype_name" kvs) as_string in
        let* pelem_type_name = Result.bind (mem "pelem_type_name" kvs) as_opt in
        let* penum_labels = Result.bind (mem "penum_labels" kvs) as_strings in
        let* pnullable =
          match List.assoc_opt "pnullable" kvs with
          | Some (`Bool b) -> Ok b
          | _ -> Diag.error "snapshot: pnullable"
        in
        Ok (pname, ptype_name, pelem_type_name, penum_labels, pnullable))
      params
  in
  let e_params =
    List.mapi
      (fun i (pname, ptype_name, pelem_type_name, penum_labels, pnullable) ->
        Describe.v_param ~index:(i + 1) ~pname ~ptype_name ~pelem_type_name ~penum_labels
          ~pnullable)
      e_params
  in
  let* columns = mem "columns" kvs in
  let* columns =
    match columns with `List l -> Ok l | _ -> Diag.error "snapshot: columns"
  in
  (* Table OIDs are database-local so the snapshot does not store them; resolve
     only groups by them, so a surrogate keyed on the table name is enough. *)
  let table_ids = Hashtbl.create 4 in
  let surrogate = function
    | None -> 0
    | Some t -> (
        match Hashtbl.find_opt table_ids t with
        | Some n -> n
        | None ->
            let n = Hashtbl.length table_ids + 1 in
            Hashtbl.add table_ids t n;
            n)
  in
  let* e_columns =
    map_result
      (fun c ->
        let* kvs = as_assoc c in
        let* cname = Result.bind (mem "name" kvs) as_string in
        let* type_name = Result.bind (mem "type_name" kvs) as_string in
        let* elem_type_name = Result.bind (mem "elem_type_name" kvs) as_opt in
        let* table = Result.bind (mem "table" kvs) as_opt in
        let* table_col =
          match List.assoc_opt "table_col" kvs with
          | Some (`Int n) -> Ok n
          | _ -> Diag.error "snapshot: table_col"
        in
        let* nullable =
          match List.assoc_opt "nullable" kvs with
          | Some (`Bool b) -> Ok b
          | _ -> Diag.error "snapshot: nullable"
        in
        let* enum_labels = Result.bind (mem "enum_labels" kvs) as_strings in
        Ok
          (Describe.v_column ~name:cname ~type_name ~elem_type_name ~table
             ~table_oid:(surrogate table) ~table_col ~nullable ~enum_labels))
      columns
  in
  let* e_model = Result.bind (mem "model_table" kvs) as_opt in
  Ok (name, { e_variants; e_params; e_columns; e_model })

let load ~queries_dir =
  let path = Filename.concat queries_dir filename in
  if not (Sys.file_exists path) then
    Diag.error "no %s in %s -- run `sqlml snapshot` against a live database to create one"
      filename queries_dir
  else
    let* json =
      match Yojson.Safe.from_file path with
      | j -> Ok j
      | exception Yojson.Json_error m -> Diag.error ~file:path "invalid JSON: %s" m
      | exception Sys_error m -> Diag.error "%s" m
    in
    let* kvs = as_assoc json in
    let* () =
      match List.assoc_opt "format_version" kvs with
      | Some (`Int v) when v = format_version -> Ok ()
      | Some (`Int v) ->
          Diag.error ~file:path
            "snapshot format v%d, this sqlml reads v%d -- re-run `sqlml snapshot`" v
            format_version
      | _ -> Diag.error ~file:path "snapshot: missing format_version"
    in
    let* files = Result.bind (mem "schema_files" kvs) as_strings in
    let* stored_hash = Result.bind (mem "schema_hash" kvs) as_opt in
    let* queries = mem "queries" kvs in
    let* queries =
      match queries with
      | `List l -> Ok l
      | _ -> Diag.error ~file:path "snapshot: queries"
    in
    let* entries = map_result entry_of_json queries in
    Ok (path, files, stored_hash, entries)

let describe_offline ~queries_dir ~config queries =
  let* path, snap_files, snap_hash, entries = load ~queries_dir in
  (* The snapshot must have been taken with the same staleness contract the
     config declares now, and the listed files must hash identically. *)
  let* () =
    if Config.schema config <> snap_files then
      Diag.error ~file:path
        "snapshot was taken with schema files [%s] but sqlml.toml now lists [%s] -- \
         re-run `sqlml snapshot`"
        (String.concat "; " snap_files)
        (String.concat "; " (Config.schema config))
    else Ok ()
  in
  let* () =
    let* current = schema_hash ~queries_dir config in
    if current <> snap_hash then
      Diag.error ~file:path
        "schema files changed since this snapshot was taken -- re-run `sqlml snapshot` \
         against a database migrated to the current schema"
    else Ok ()
  in
  map_result
    (fun (q : Parse.t) ->
      let fail fmt =
        Printf.ksprintf
          (fun m ->
            Error (Diag.v ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name m))
          fmt
      in
      match List.assoc_opt q.Parse.name entries with
      | None -> fail "not in the snapshot -- re-run `sqlml snapshot`"
      | Some e ->
          if List.map hash (variant_sqls q) <> e.e_variants then
            fail "SQL changed since the snapshot was taken -- re-run `sqlml snapshot`"
          else
            Ok
              (Describe.v_described ~query:q ~params:e.e_params ~columns:e.e_columns
                 ~model_table:e.e_model))
    queries
