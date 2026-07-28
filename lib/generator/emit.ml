(* Turning described queries into OCaml source.

   One module per queries directory (not per .sql file), so shared model types
   work across files and callers need a single [open Db]. *)

let ( let* ) = Result.bind

type field =
  { fname : string
  ; ftype : Typemap.t
  }

(* ---------- resolution ---------- *)

let resolve_column (q : Parse.t) (c : Describe.column) =
  match
    Typemap.of_pg ~type_name:c.Describe.type_name ~enum_labels:c.Describe.enum_labels
      ~nullable:c.Describe.nullable
  with
  | Some t -> Ok { fname = c.Describe.name; ftype = t }
  | None ->
    Error
      (Printf.sprintf "%s:%d: %s: column %S has Postgres type %S, which sqlml has no mapping for"
         q.Parse.file q.Parse.line q.Parse.name c.Describe.name c.Describe.type_name)

let resolve_param (q : Parse.t) (p : Describe.param) =
  match
    Typemap.of_pg ~type_name:p.Describe.ptype_name ~enum_labels:p.Describe.penum_labels
      ~nullable:p.Describe.pnullable
  with
  | Some t -> Ok { fname = p.Describe.pname; ftype = t }
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

type resolved =
  { d : Describe.described
  ; row_type : string option (* None for :exec *)
  ; shared : bool
  ; cols : field list
  ; ps : field list
  }

let row_type_name (d : Describe.described) =
  match d.Describe.model_table with
  | Some t -> Parse.to_snake t ^ "_row"
  | None -> Parse.to_snake d.Describe.query.Parse.name ^ "_row"

let resolve (d : Describe.described) =
  let q = d.Describe.query in
  let* cols = map_result (resolve_column q) d.Describe.columns in
  let* ps = map_result (resolve_param q) d.Describe.params in
  let row_type =
    match q.Parse.cardinality with Parse.Exec -> None | _ -> Some (row_type_name d)
  in
  Ok { d; row_type; shared = d.Describe.model_table <> None; cols; ps }

(* ---------- collecting shared pieces ---------- *)

let collect_enums resolved =
  let tbl = Hashtbl.create 8 in
  let add = function
    | Typemap.Enum (n, labels) | Typemap.Option (Typemap.Enum (n, labels)) ->
      (match Hashtbl.find_opt tbl n with
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
      match r.row_type with
      | None -> go tl
      | Some name -> (
        match Hashtbl.find_opt seen name with
        | None ->
          Hashtbl.replace seen name r.cols;
          order := (name, r.cols) :: !order;
          go tl
        | Some prev when prev = r.cols -> go tl
        | Some _ ->
          Error
            (Printf.sprintf
               "%s: queries %S and an earlier one both map to row type %S but disagree on its \
                fields"
               r.d.Describe.query.Parse.file r.d.Describe.query.Parse.name name)))
  in
  go resolved

(* ---------- emitting ---------- *)

let bprintf = Printf.bprintf

let emit_record b name fields =
  bprintf b "type %s =\n" name;
  List.iteri
    (fun i f ->
      bprintf b "  %c %s : %s\n" (if i = 0 then '{' else ';') f.fname (Typemap.ocaml_type f.ftype))
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
  | Parse.Many -> Printf.sprintf "%s list" (Option.get r.row_type)
  | Parse.Exec -> "int"

let runner r =
  match r.d.Describe.query.Parse.cardinality with
  | Parse.One -> "Sqlml.fetch_one"
  | Parse.Many -> "Sqlml.fetch_all"
  | Parse.Exec -> "Sqlml.exec"

let query_sig r =
  match r.d.Describe.query.Parse.cardinality with
  | Parse.One -> "Sqlml.Query.ONE"
  | Parse.Many -> "Sqlml.Query.MANY"
  | Parse.Exec -> "Sqlml.Query.EXEC"

let fn_name r = Parse.to_snake r.d.Describe.query.Parse.name
let mod_name r = r.d.Describe.query.Parse.module_name

let emit_signature b r =
  let m = mod_name r in
  let q = r.d.Describe.query in
  bprintf b "module %s : sig\n" m;
  (if r.ps = [] then bprintf b "  type params = unit\n\n"
   else begin
     bprintf b "  type params =\n";
     List.iteri
       (fun i f ->
         bprintf b "    %c %s : %s\n" (if i = 0 then '{' else ';') f.fname
           (Typemap.ocaml_type f.ftype))
       r.ps;
     bprintf b "    }\n\n"
   end);
  (match r.row_type with
   | Some row -> bprintf b "  include %s with type params := params and type row = %s\n" (query_sig r) row
   | None -> bprintf b "  include %s with type params := params\n" (query_sig r));
  bprintf b "end\n\n";
  let mandatory, optional = split_args r.ps in
  let args = List.map arg_sig (mandatory @ optional) in
  let args = if optional = [] then args else args @ [ "unit" ] in
  let chain = String.concat " -> " ("Sqlml.conn" :: args) in
  emit_doc b q;
  bprintf b "val %s : %s -> (%s, Sqlml.Error.t) result\n\n" (fn_name r) chain (result_type r);
  bprintf b "(** Raising {!%s}.\n    @raise Sqlml.Sql_error on connection, execution or decode failure. *)\n"
    (fn_name r);
  bprintf b "val %s_exn : %s -> %s\n\n" (fn_name r) chain (result_type r)

let emit_implementation b r =
  let m = mod_name r in
  let q = r.d.Describe.query in
  bprintf b "module %s = struct\n" m;
  (if r.ps = [] then bprintf b "  type params = unit\n"
   else begin
     bprintf b "  type params =\n";
     List.iteri
       (fun i f ->
         bprintf b "    %c %s : %s\n" (if i = 0 then '{' else ';') f.fname
           (Typemap.ocaml_type f.ftype))
       r.ps;
     bprintf b "    }\n"
   end);
  (match r.row_type with Some row -> bprintf b "  type row = %s\n" row | None -> ());
  bprintf b "\n  let name = %S\n" q.Parse.name;
  bprintf b "  let sql = %S\n\n" q.Parse.sql;
  (* [p.field] rather than a pattern: params and row routinely share field names
     and OCaml would resolve an unannotated pattern to the later type *)
  if r.ps = [] then bprintf b "  let encode () = []\n"
  else begin
    bprintf b "  let encode (p : params) =\n";
    List.iteri
      (fun i f ->
        bprintf b "    %c %s p.%s\n" (if i = 0 then '[' else ';') (Typemap.encoder f.ftype) f.fname)
      r.ps;
    bprintf b "    ]\n"
  end;
  (match r.row_type with
   | None -> ()
   | Some row ->
     bprintf b "\n  let decode r : %s =\n" row;
     List.iteri
       (fun i f ->
         bprintf b "    %c %s = %s r %d\n" (if i = 0 then '{' else ';') f.fname
           (Typemap.decoder f.ftype) i)
       r.cols;
     bprintf b "    }\n");
  bprintf b "end\n\n";
  let mandatory, optional = split_args r.ps in
  let all = mandatory @ optional in
  let uses =
    match all with [] -> "" | _ -> " " ^ String.concat " " (List.map arg_use all)
  in
  let tail = if optional = [] then "" else " ()" in
  let param_value =
    if r.ps = [] then "()"
    else
      "{ " ^ m ^ "."
      ^ String.concat "; " (List.map (fun f -> f.fname) r.ps)
      ^ " }"
  in
  bprintf b "let %s conn%s%s = %s {%s} conn %s\n" (fn_name r) uses tail (runner r) m param_value;
  bprintf b "let %s_exn conn%s%s = Sqlml.or_raise (%s conn%s%s)\n\n" (fn_name r) uses tail
    (fn_name r) uses tail

let header src =
  Printf.sprintf
    "(* Generated by sqlml from %s -- do not edit.\n\n   Regenerate with: sqlml generate *)\n\n" src

let generate ~src (described : Describe.described list) =
  let* resolved = map_result resolve described in
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
      List.iter (fun l -> bprintf ml "  | %s -> %S\n" (Typemap.constructor_of_label l) l) labels;
      bprintf ml "\n";
      bprintf ml "let %s_to_value x = Sqlml.Value.of_string (%s_to_string x)\n" name name;
      bprintf ml "let _ = %s_to_value\n\n" name;
      bprintf ml "let %s_of_row r i =\n  match Sqlml.Row.string r i with\n" name;
      List.iter (fun l -> bprintf ml "  | %S -> %s\n" l (Typemap.constructor_of_label l)) labels;
      bprintf ml
        "  | o -> raise (Sqlml.Row.Bad { column = i; expected = %S; got = o })\n\n" name)
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
