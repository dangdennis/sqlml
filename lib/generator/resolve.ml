(** Semantic analysis: what to generate.

    Turns described queries into [resolved] values with every decision made -- OCaml types
    chosen against the config, field and row-type names fixed, collisions rejected,
    optional-block parameters attributed. [Render] then only prints. Keeping the phases
    apart means naming and collision logic can be tested without ever rendering, and the
    renderer stays boring. *)

open Gen_util

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
  let fname = field_name cfg ~table ~name:c.Describe.name in
  if not (Gen_util.is_lower_ident fname) then
    Diag.error ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name
      "column %S is not usable as an OCaml field name%s; alias it in the SELECT list (AS \
       some_name) or rename it in sqlml.toml"
      fname
      (if List.mem fname Gen_util.ocaml_keywords then " (OCaml keyword)" else "")
  else
    match
      Typemap.of_pg ?custom ~type_name:c.Describe.type_name
        ~elem_type_name:c.Describe.elem_type_name ~enum_labels:c.Describe.enum_labels
        ~nullable:c.Describe.nullable ()
    with
    | Some t -> Ok { fname; ftype = t; ord = c.Describe.table_col }
    | None ->
        Diag.error ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name
          "column %S has Postgres type %S, which sqlml has no mapping for" c.Describe.name
          c.Describe.type_name

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
  | Some t when not (Gen_util.is_lower_ident p.Describe.pname) ->
      ignore t;
      Diag.error ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name
        "parameter %S is not usable as an OCaml argument name%s; rename the :param"
        p.Describe.pname
        (if List.mem p.Describe.pname Gen_util.ocaml_keywords then " (OCaml keyword)"
         else "")
  | Some t ->
      let t = if in_block then Typemap.Option t else t in
      Ok { fname = p.Describe.pname; ftype = t; ord = 0 }
  | None ->
      Diag.error ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name
        "parameter %S has Postgres type %S, which sqlml has no mapping for"
        p.Describe.pname p.Describe.ptype_name

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
  let conflict = ref None in
  let bad_name = ref None in
  let add ~ctx = function
    | Typemap.Enum (n, labels)
    | Typemap.Option (Typemap.Enum (n, labels))
    | Typemap.Array (Typemap.Enum (n, labels))
    | Typemap.Option (Typemap.Array (Typemap.Enum (n, labels))) -> (
        if (not (Gen_util.is_lower_ident n)) && !bad_name = None then
          bad_name := Some (n, ctx);
        match Hashtbl.find_opt tbl n with
        | Some existing when existing <> labels ->
            (* typnames are schema-scoped: two schemas on the search path can
               both define an enum called `status` with different labels, and
               decoding one with the other's constructors would be silently
               wrong data *)
            if !conflict = None then conflict := Some (n, ctx)
        | _ -> Hashtbl.replace tbl n labels)
    | _ -> ()
  in
  List.iter
    (fun r ->
      let ctx = r.d.Describe.query in
      List.iter (fun f -> add ~ctx f.ftype) r.cols;
      List.iter (fun f -> add ~ctx f.ftype) r.ps)
    resolved;
  match (!bad_name, !conflict) with
  | Some (n, q), _ ->
      Diag.error ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name
        "enum type %S is not usable as an OCaml type name; rename it in sqlml.toml" n
  | _, Some (n, q) ->
      Diag.error ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name
        "two different enums named %S (schema-scoped typnames?) reach this module with \
         different labels; qualify or rename one in sqlml.toml"
        n
  | None, None ->
      Ok (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [] |> List.sort compare)

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
                Diag.error ~file:r.d.Describe.query.Parse.file
                  ~line:r.d.Describe.query.Parse.line
                  "row type %S is claimed by both %s and %s, with different fields.\n\
                  \  Rename the query, or select the table's full row so they share the \
                   model."
                  name (describe_origin prev_origin) (describe_origin origin)))
  in
  go resolved

(* ---------- emitting ---------- *)
