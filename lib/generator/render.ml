(* See render.mli. No decisions are made here; anything requiring judgement
   belongs in [Resolve]. *)

open Resolve

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

(* A SQL docstring containing a comment terminator, or an unbalanced comment
   opener, would break the generated (** ... *) block; neutralise both by
   splitting the two-character sequences. *)
let sanitize_doc line =
  let b = Buffer.create (String.length line) in
  String.iteri
    (fun i c ->
      if c = '*' && i + 1 < String.length line && line.[i + 1] = ')' then
        Buffer.add_string b "* "
      else if c = '(' && i + 1 < String.length line && line.[i + 1] = '*' then
        Buffer.add_string b "( "
      else Buffer.add_char b c)
    line;
  Buffer.contents b

let emit_doc b (q : Parse.t) =
  match q.Parse.doc with
  | [] -> ()
  | lines ->
      bprintf b "(** %s *)\n" (String.concat "\n    " (List.map sanitize_doc lines))

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

let source ~src ~enums ~rows resolved =
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
  (Buffer.contents mli, Buffer.contents ml)
