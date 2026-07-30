(** Turning described queries into OCaml source.

    One module per queries directory (not per .sql file), so shared model types work
    across files and callers need a single [open Db]. *)

let ( let* ) = Result.bind

type field = {
  fname : string;
  ftype : Typemap.t;
  ord : int; (* attnum for a table column, 0 otherwise *)
}

(* ---------- resolution ---------- *)

let custom_of_config cfg ~key ~pg_type =
  Config.custom cfg ~key ~pg_type
  |> Option.map (fun (c : Config.custom) ->
      {
        Typemap.c_ocaml = c.Config.ocaml;
        c_of_string = c.Config.of_string;
        c_to_string = c.Config.to_string;
      })

(* A field can be renamed by table.column, so two queries selecting the same
   column agree on what it is called. *)
let field_name cfg ~table ~name =
  match table with
  | Some t -> Option.value (Config.renamed cfg (t ^ "." ^ name)) ~default:name
  | None -> name

let resolve_column cfg (q : Parse.t) (c : Describe.column) =
  let table = c.Describe.table in
  let custom =
    custom_of_config cfg
      ~key:(Option.map (fun t -> t ^ "." ^ c.Describe.name) table)
      ~pg_type:c.Describe.type_name
  in
  match
    Typemap.of_pg ?custom ~type_name:c.Describe.type_name
      ~elem_type_name:c.Describe.elem_type_name ~enum_labels:c.Describe.enum_labels
      ~nullable:c.Describe.nullable ()
  with
  | Some t ->
      Ok
        {
          fname = field_name cfg ~table ~name:c.Describe.name;
          ftype = t;
          ord = c.Describe.table_col;
        }
  | None ->
      Error
        (Printf.sprintf
           "%s:%d: %s: column %S has Postgres type %S, which sqlml has no mapping for"
           q.Parse.file q.Parse.line q.Parse.name c.Describe.name c.Describe.type_name)

let block_param_names (q : Parse.t) =
  match q.Parse.dynamic with
  | None -> []
  | Some d -> Array.to_list d.Parse.block_params |> List.concat

let resolve_param cfg (q : Parse.t) (p : Describe.param) =
  let in_block = List.mem p.Describe.pname (block_param_names q) in
  let custom =
    custom_of_config cfg
      ~key:(Some (q.Parse.name ^ "." ^ p.Describe.pname))
      ~pg_type:p.Describe.ptype_name
  in
  match
    Typemap.of_pg ?custom ~type_name:p.Describe.ptype_name
      ~elem_type_name:p.Describe.pelem_type_name ~enum_labels:p.Describe.penum_labels
      ~nullable:p.Describe.pnullable ()
  with
  (* a block parameter is optional by construction: Option in the record and
     an optional argument, but encoded by omission rather than as NULL *)
  | Some t ->
      let t = if in_block then Typemap.Option t else t in
      Ok { fname = p.Describe.pname; ftype = t; ord = 0 }
  | None ->
      Error
        (Printf.sprintf
           "%s:%d: %s: parameter %S has Postgres type %S, which sqlml has no mapping for"
           q.Parse.file q.Parse.line q.Parse.name p.Describe.pname p.Describe.ptype_name)

let rec map_result f = function
  | [] -> Ok []
  | x :: tl ->
      let* y = f x in
      let* rest = map_result f tl in
      Ok (y :: rest)

(* Where a generated row type name came from, so a collision can say which two
   things collided rather than blaming the wrong one. *)
type origin =
  | From_table of string (* shared model for a table *)
  | From_query of string (* named after the query *)

let describe_origin = function
  | From_table t -> Printf.sprintf "the shared model for table %s" t
  | From_query q -> Printf.sprintf "query %s" q

type resolved = {
  d : Describe.described;
  row_type : string option (* None for :exec *);
  row_origin : origin option;
  shared : bool;
  cols : field list (* SELECT order -- decoders index by position *);
  type_fields : field list (* order the record type is declared in *);
  ps : field list;
}

let row_type_name cfg (d : Describe.described) =
  match d.Describe.model_table with
  | Some t ->
      let base = Option.value (Config.renamed cfg t) ~default:t in
      (Parse.to_snake base ^ "_row", From_table t)
  | None ->
      ( Parse.to_snake d.Describe.query.Parse.name ^ "_row",
        From_query d.Describe.query.Parse.name )

let resolve cfg (d : Describe.described) =
  let q = d.Describe.query in
  let* cols = map_result (resolve_column cfg q) d.Describe.columns in
  let* ps = map_result (resolve_param cfg q) d.Describe.params in
  let row_type =
    match q.Parse.cardinality with Parse.Exec -> None | _ -> Some (row_type_name cfg d)
  in
  let row_origin = Option.map snd row_type in
  let row_type = Option.map fst row_type in
  let shared = d.Describe.model_table <> None in
  (* A shared model must not depend on the order columns happen to appear in one
     query's SELECT list: two queries selecting the same table's full row in
     different orders describe the same type. Canonicalise on attnum. Decoders
     are unaffected -- they build the record by field name from [cols]. *)
  let type_fields =
    if shared then List.stable_sort (fun a b -> compare a.ord b.ord) cols else cols
  in
  Ok { d; row_type; row_origin; shared; cols; type_fields; ps }

(* ---------- collecting shared pieces ---------- *)

let collect_enums resolved =
  let tbl = Hashtbl.create 8 in
  let add = function
    | Typemap.Enum (n, labels)
    | Typemap.Option (Typemap.Enum (n, labels))
    | Typemap.Array (Typemap.Enum (n, labels))
    | Typemap.Option (Typemap.Array (Typemap.Enum (n, labels))) -> (
        match Hashtbl.find_opt tbl n with
        | Some existing when existing <> labels ->
            (* same type name with different labels cannot happen from one database *)
            ()
        | _ -> Hashtbl.replace tbl n labels)
    | _ -> ()
  in
  List.iter
    (fun r ->
      List.iter (fun f -> add f.ftype) r.cols;
      List.iter (fun f -> add f.ftype) r.ps)
    resolved;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [] |> List.sort compare

(* Row types to emit, in first-seen order, deduplicated by name. A shared model
   appearing in several queries is emitted once; a mismatch in field types
   between two queries claiming the same model is an error rather than a
   silent pick. *)
let collect_rows resolved =
  let seen = Hashtbl.create 8 in
  let order = ref [] in
  let rec go = function
    | [] -> Ok (List.rev !order)
    | r :: tl -> (
        match (r.row_type, r.row_origin) with
        | None, _ | _, None -> go tl
        | Some name, Some origin -> (
            match Hashtbl.find_opt seen name with
            | None ->
                Hashtbl.replace seen name (r.type_fields, origin);
                order := (name, r.type_fields) :: !order;
                go tl
            (* same name, same fields: the shared model doing its job *)
            | Some (prev, _) when prev = r.type_fields -> go tl
            | Some (_, prev_origin) ->
                Error
                  (Printf.sprintf
                     "%s:%d: row type %S is claimed by both %s and %s, with different \
                      fields.\n\
                     \  Rename the query, or select the table's full row so they share \
                      the model."
                     r.d.Describe.query.Parse.file r.d.Describe.query.Parse.line name
                     (describe_origin prev_origin) (describe_origin origin))))
  in
  go resolved

(* ---------- emitting ---------- *)

let bprintf = Printf.bprintf

let emit_record b name fields =
  bprintf b "type %s =\n" name;
  List.iteri
    (fun i f ->
      bprintf b "  %c %s : %s\n"
        (if i = 0 then '{' else ';')
        f.fname (Typemap.ocaml_type f.ftype))
    fields;
  bprintf b "  }\n\n"

let emit_enum_types b enums =
  List.iter
    (fun (name, labels) ->
      bprintf b "type %s =\n" name;
      List.iter (fun l -> bprintf b "  | %s\n" (Typemap.constructor_of_label l)) labels;
      bprintf b "\n")
    enums

let emit_doc b (q : Parse.t) =
  match q.Parse.doc with
  | [] -> ()
  | lines -> bprintf b "(** %s *)\n" (String.concat "\n    " lines)

(* mandatory args first (in SQL order), then optional; a trailing unit only when
   there is at least one optional *)
let split_args ps = List.partition (fun f -> not (Typemap.is_option f.ftype)) ps

let arg_sig f =
  if Typemap.is_option f.ftype then
    Printf.sprintf "?%s:%s" f.fname (Typemap.ocaml_type (Typemap.strip_option f.ftype))
  else Printf.sprintf "%s:%s" f.fname (Typemap.ocaml_type f.ftype)

let arg_use f = (if Typemap.is_option f.ftype then "?" else "~") ^ f.fname

let result_type r =
  match r.d.Describe.query.Parse.cardinality with
  | Parse.One -> Printf.sprintf "%s option" (Option.get r.row_type)
  | Parse.One_strict -> Option.get r.row_type
  | Parse.Many -> Printf.sprintf "%s list" (Option.get r.row_type)
  | Parse.Exec -> "int"

let runner r =
  match r.d.Describe.query.Parse.cardinality with
  | Parse.One -> "Sqlml.fetch_one"
  | Parse.One_strict -> "Sqlml.fetch_one_strict"
  | Parse.Many -> "Sqlml.fetch_all"
  | Parse.Exec -> "Sqlml.exec"

let query_sig r =
  match r.d.Describe.query.Parse.cardinality with
  | Parse.One -> "Sqlml.Query.ONE"
  | Parse.One_strict -> "Sqlml.Query.ONE_STRICT"
  | Parse.Many -> "Sqlml.Query.MANY"
  | Parse.Exec -> "Sqlml.Query.EXEC"

let fn_name r = Parse.to_snake r.d.Describe.query.Parse.name
let mod_name r = r.d.Describe.query.Parse.module_name

let emit_signature b r =
  let m = mod_name r in
  let q = r.d.Describe.query in
  bprintf b "module %s : sig\n" m;
  if r.ps = [] then bprintf b "  type params = unit\n\n"
  else begin
    bprintf b "  type params =\n";
    List.iteri
      (fun i f ->
        bprintf b "    %c %s : %s\n"
          (if i = 0 then '{' else ';')
          f.fname (Typemap.ocaml_type f.ftype))
      r.ps;
    bprintf b "    }\n\n"
  end;
  (match r.row_type with
  | Some row ->
      bprintf b "  include %s with type params := params and type row = %s\n"
        (query_sig r) row
  | None -> bprintf b "  include %s with type params := params\n" (query_sig r));
  bprintf b "end\n\n";
  let mandatory, optional = split_args r.ps in
  let args = List.map arg_sig (mandatory @ optional) in
  let args = if optional = [] then args else args @ [ "unit" ] in
  let chain = String.concat " -> " ("Sqlml.conn" :: args) in
  emit_doc b q;
  bprintf b "val %s : %s -> (%s, Sqlml.Error.t) result\n\n" (fn_name r) chain
    (result_type r);
  bprintf b
    "(** Raising {!%s}.\n\
    \    @raise Sqlml.Sql_error on connection, execution or decode failure. *)\n"
    (fn_name r);
  bprintf b "val %s_exn : %s -> %s\n\n" (fn_name r) chain (result_type r)

let emit_implementation b r =
  let m = mod_name r in
  let q = r.d.Describe.query in
  bprintf b "module %s = struct\n" m;
  if r.ps = [] then bprintf b "  type params = unit\n"
  else begin
    bprintf b "  type params =\n";
    List.iteri
      (fun i f ->
        bprintf b "    %c %s : %s\n"
          (if i = 0 then '{' else ';')
          f.fname (Typemap.ocaml_type f.ftype))
      r.ps;
    bprintf b "    }\n"
  end;
  (match r.row_type with Some row -> bprintf b "  type row = %s\n" row | None -> ());
  bprintf b "\n  let name = %S\n" q.Parse.name;
  (match q.Parse.dynamic with
  | None -> bprintf b "  let sql (_ : params) = %S\n\n" q.Parse.sql
  | Some d ->
      (* one pre-verified SQL text per inclusion combination, indexed by which
         optional blocks are active *)
      bprintf b "\n  let variants =\n    [|\n";
      Array.iter (fun v -> bprintf b "      %S;\n" v) d.Parse.variant_sqls;
      bprintf b "    |]\n");
  (match q.Parse.dynamic with
  | None -> ()
  | Some d ->
      bprintf b "\n  let sql (p : params) =\n";
      Array.iteri
        (fun k bp ->
          match bp with
          | [ one ] ->
              bprintf b
                "    let b%d = match p.%s with Some _ -> true | None -> false in\n" k one
          | many ->
              let somes = String.concat ", " (List.map (fun _ -> "Some _") many) in
              let nones = String.concat ", " (List.map (fun _ -> "None") many) in
              let tuple = String.concat ", " (List.map (fun n -> "p." ^ n) many) in
              bprintf b
                "    let b%d =\n\
                \      match (%s) with\n\
                \      | %s -> true\n\
                \      | %s -> false\n\
                \      | _ -> invalid_arg %S\n\
                \    in\n"
                k tuple somes nones
                (Printf.sprintf "%s: parameters %s must be supplied together" q.Parse.name
                   (String.concat ", " many)))
        d.Parse.block_params;
      let mask =
        String.concat " lor "
          (List.init d.Parse.nblocks (fun k ->
               Printf.sprintf "(if b%d then %d else 0)" k (1 lsl k)))
      in
      bprintf b "    variants.(%s)\n\n" mask);
  (* [p.field] rather than a pattern: params and row routinely share field names
     and OCaml would resolve an unannotated pattern to the later type *)
  let blocks = block_param_names q in
  if r.ps = [] then bprintf b "  let encode () = []\n"
  else if blocks = [] then begin
    bprintf b "  let encode (p : params) =\n";
    List.iteri
      (fun i f ->
        bprintf b "    %c %s p.%s\n"
          (if i = 0 then '[' else ';')
          (Typemap.encoder f.ftype) f.fname)
      r.ps;
    bprintf b "    ]\n"
  end
  else begin
    (* document order, omitting inactive blocks entirely: the chosen variant's
       $n numbering is the full variant's with absent parameters deleted *)
    bprintf b "  let encode (p : params) =\n    List.concat\n";
    List.iteri
      (fun i f ->
        let br = if i = 0 then '[' else ';' in
        if List.mem f.fname blocks then
          bprintf b "      %c (match p.%s with None -> [] | Some v -> [ %s v ])\n" br
            f.fname
            (Typemap.encoder (Typemap.strip_option f.ftype))
        else bprintf b "      %c [ %s p.%s ]\n" br (Typemap.encoder f.ftype) f.fname)
      r.ps;
    bprintf b "      ]\n"
  end;
  (match r.row_type with
  | None -> ()
  | Some row ->
      bprintf b "\n  let decode r : %s =\n" row;
      List.iteri
        (fun i f ->
          bprintf b "    %c %s = %s r %d\n"
            (if i = 0 then '{' else ';')
            f.fname (Typemap.decoder f.ftype) i)
        r.cols;
      bprintf b "    }\n");
  (match r.row_type with
  | Some _ -> bprintf b "\n  let columns = %d\n" (List.length r.cols)
  | None -> ());
  bprintf b "\n  let cardinality = %s\n"
    (match r.d.Describe.query.Parse.cardinality with
    | Parse.One -> "Sqlml.Query.One"
    | Parse.One_strict -> "Sqlml.Query.One_strict"
    | Parse.Many -> "Sqlml.Query.Many"
    | Parse.Exec -> "Sqlml.Query.Exec");
  bprintf b "end\n\n";
  let mandatory, optional = split_args r.ps in
  let all = mandatory @ optional in
  let uses =
    match all with [] -> "" | _ -> " " ^ String.concat " " (List.map arg_use all)
  in
  let tail = if optional = [] then "" else " ()" in
  let param_value =
    if r.ps = [] then "()"
    else "{ " ^ m ^ "." ^ String.concat "; " (List.map (fun f -> f.fname) r.ps) ^ " }"
  in
  bprintf b "let %s conn%s%s = %s (module %s) conn %s\n" (fn_name r) uses tail (runner r)
    m param_value;
  bprintf b "let %s_exn conn%s%s = Sqlml.or_raise (%s conn%s%s)\n\n" (fn_name r) uses tail
    (fn_name r) uses tail

let header src =
  Printf.sprintf
    "(* Generated by sqlml from %s -- do not edit.\n\n\
    \   Regenerate with: sqlml generate *)\n\n"
    src

let generate ?(config = Config.empty) ~src (described : Describe.described list) =
  let* resolved = map_result (resolve config) described in
  let* rows = collect_rows resolved in
  let enums = collect_enums resolved in
  let mli = Buffer.create 4096 in
  let ml = Buffer.create 8192 in
  Buffer.add_string mli (header src);
  Buffer.add_string ml (header src);
  (* generated row records have fields the caller may not read; warning 69 fires
     in the defining module, which the caller cannot fix *)
  Buffer.add_string ml "[@@@warning \"-69\"]\n\n";
  emit_enum_types mli enums;
  emit_enum_types ml enums;
  List.iter
    (fun (name, labels) ->
      bprintf mli "val %s_to_string : %s -> string\n\n" name name;
      bprintf ml "let %s_to_string = function\n" name;
      List.iter
        (fun l -> bprintf ml "  | %s -> %S\n" (Typemap.constructor_of_label l) l)
        labels;
      bprintf ml "\n";
      bprintf ml "let %s_to_value x = Sqlml.Value.of_string (%s_to_string x)\n" name name;
      bprintf ml "let _ = %s_to_value\n\n" name;
      bprintf ml "let %s_of_string = function\n" name;
      List.iter
        (fun l -> bprintf ml "  | %S -> %s\n" l (Typemap.constructor_of_label l))
        labels;
      bprintf ml "  | o -> failwith (%S ^ \": \" ^ o)\n\n" name;
      bprintf ml "let %s_of_row r i =\n" name;
      bprintf ml "  let s = Sqlml.Row.string r i in\n";
      bprintf ml "  try %s_of_string s\n" name;
      bprintf ml
        "  with _ -> raise (Sqlml.Row.Bad { column = i; expected = %S; got = s })\n\n"
        name)
    enums;
  List.iter
    (fun (name, fields) ->
      emit_record mli name fields;
      emit_record ml name fields)
    rows;
  List.iter
    (fun r ->
      emit_signature mli r;
      emit_implementation ml r)
    resolved;
  Ok (Buffer.contents mli, Buffer.contents ml)
