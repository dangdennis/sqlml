(* See resolve.mli. Naming and collision logic lives here, apart from
   [Render], so it can be tested without ever rendering and the renderer stays
   boring. *)

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
      { Typemap.c_ocaml = c.ocaml; c_of_string = c.of_string; c_to_string = c.to_string })

let rename_type cfg id =
  Option.value
    (Config.renamed cfg (Pg_type.qualified id))
    ~default:(Pg_type.generated_name id)

let map_type cfg ~key pg ~nullable =
  let custom id = custom_of_config cfg ~key:None ~pg_type:(Pg_type.qualified id) in
  match
    Option.bind key (fun key ->
        custom_of_config cfg ~key:(Some key) ~pg_type:"__no_type__")
  with
  | Some c ->
      Some (if nullable then Typemap.Option (Typemap.Custom c) else Typemap.Custom c)
  | None -> Typemap.of_type ~custom ~rename:(rename_type cfg) pg ~nullable

(* A field can be renamed by table.column, so two queries selecting the same
   column agree on what it is called. *)
let field_name cfg ~table ~name =
  match table with
  | Some t -> Option.value (Config.renamed cfg (t ^ "." ^ name)) ~default:name
  | None -> name

let resolve_column cfg (q : Parse.t) (c : Describe.column) =
  let table = c.Describe.table in
  let fname = field_name cfg ~table ~name:c.Describe.name in
  if not (Gen_util.is_lower_ident fname) then
    Diag.error ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name
      "column %S is not usable as an OCaml field name%s; alias it in the SELECT list (AS \
       some_name) or rename it in sqlml.toml"
      fname
      (if List.mem fname Gen_util.ocaml_keywords then " (OCaml keyword)" else "")
  else
    match
      map_type cfg
        ~key:(Option.map (fun t -> t ^ "." ^ c.Describe.name) table)
        c.pg_type ~nullable:c.nullable
    with
    | Some t -> Ok { fname; ftype = t; ord = c.Describe.table_col }
    | None ->
        Diag.error ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name
          "column %S has Postgres type %S, which sqlml has no mapping for (anonymous \
           records must be cast to a named composite)"
          c.Describe.name
          (Pg_type.qualified c.Describe.pg_type.id)

let block_param_names (q : Parse.t) =
  match q.Parse.dynamic with
  | None -> []
  | Some d -> Array.to_list d.Parse.block_params |> List.concat

let resolve_param cfg (q : Parse.t) (p : Describe.param) =
  let in_block = List.mem p.Describe.pname (block_param_names q) in
  match
    map_type cfg
      ~key:(Some (q.Parse.name ^ "." ^ p.Describe.pname))
      p.pg_type ~nullable:p.pnullable
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
        p.Describe.pname
        (Pg_type.qualified p.Describe.pg_type.id)

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
  row_type : string option (* None for :exec and :copy *);
  row_origin : origin option;
  cols : field list (* SELECT order -- decoders index by position *);
  type_fields : field list (* order the record type is declared in *);
  ps : field list;
  copy : (string * string list) option (* :copy target table and columns *);
}

let row_type_name cfg (d : Describe.described) =
  match d.Describe.model_table with
  | Some t ->
      let base = Option.value (Config.renamed cfg t) ~default:t in
      ( Parse.to_snake (String.map (fun c -> if c = '.' then '_' else c) base) ^ "_row",
        From_table t )
  | None ->
      ( Parse.to_snake d.Describe.query.Parse.name ^ "_row",
        From_query d.Describe.query.Parse.name )

(* The one SQL shape :copy accepts, extracted syntactically AFTER the whole
   statement was verified by Describe: INSERT INTO t (c1..cn) VALUES ($1..$n),
   one plain placeholder per column, nothing else. The COPY statement is built
   from this column list at codegen, so no SQL is ever assembled at runtime
   that Describe has not blessed the types of. *)
let copy_target (q : Parse.t) =
  let fail fmt =
    Printf.ksprintf
      (fun m ->
        Error
          (Diag.v ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name
             (m ^ " -- a :copy query must be exactly INSERT INTO t (col, ...) VALUES "
            ^ "(:param, ...), one parameter per column")))
      fmt
  in
  let sql = q.Parse.sql in
  let n = String.length sql in
  let pos = ref 0 in
  let skip_ws () =
    while
      !pos < n && match sql.[!pos] with ' ' | '\t' | '\n' | '\r' -> true | _ -> false
    do
      incr pos
    done
  in
  let word () =
    skip_ws ();
    let s = !pos in
    while
      !pos < n
      &&
      match sql.[!pos] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '"' -> true
      | _ -> false
    do
      incr pos
    done;
    String.sub sql s (!pos - s)
  in
  let expect_char c what =
    skip_ws ();
    if !pos < n && sql.[!pos] = c then begin
      incr pos;
      Ok ()
    end
    else fail "expected %s" what
  in
  let expect_kw kw =
    let w = word () in
    if String.lowercase_ascii w = kw then Ok ()
    else fail "expected %s" (String.uppercase_ascii kw)
  in
  let* () = expect_kw "insert" in
  let* () = expect_kw "into" in
  let table = word () in
  let* () = if table = "" then fail "expected a table name" else Ok () in
  let* () = expect_char '(' "( before the column list" in
  let rec columns acc =
    let c = word () in
    if c = "" then fail "expected a column name"
    else begin
      skip_ws ();
      if !pos < n && sql.[!pos] = ',' then begin
        incr pos;
        columns (c :: acc)
      end
      else
        let* () = expect_char ')' ") after the column list" in
        Ok (List.rev (c :: acc))
    end
  in
  let* cols = columns [] in
  let* () = expect_kw "values" in
  let* () = expect_char '(' "( before the VALUES list" in
  let rec placeholders k =
    let w = word () in
    if w <> "" then fail "column %d of VALUES must be a plain parameter, got %S" k w
    else
      let* () = expect_char '$' (Printf.sprintf "$%d" k) in
      let s = !pos in
      while !pos < n && match sql.[!pos] with '0' .. '9' -> true | _ -> false do
        incr pos
      done;
      let num = String.sub sql s (!pos - s) in
      if num <> string_of_int k then fail "parameters must appear in order ($%d next)" k
      else begin
        skip_ws ();
        if !pos < n && sql.[!pos] = ',' then begin
          incr pos;
          placeholders (k + 1)
        end
        else
          let* () = expect_char ')' ") after the VALUES list" in
          Ok k
      end
  in
  let* nvals = placeholders 1 in
  skip_ws ();
  let* () =
    if !pos <> n then fail "unexpected trailing SQL (RETURNING is not supported)"
    else Ok ()
  in
  if List.length cols <> nvals then
    fail "%d columns but %d parameters" (List.length cols) nvals
  else if nvals <> List.length q.Parse.params then
    fail "each parameter must be used exactly once"
  else Ok (table, cols)

let resolve cfg (d : Describe.described) =
  let q = d.Describe.query in
  let* cols = map_result (resolve_column cfg q) d.Describe.columns in
  let* ps = map_result (resolve_param cfg q) d.Describe.params in
  let* copy =
    match q.Parse.cardinality with
    | Parse.Copy ->
        let* () =
          if d.Describe.columns <> [] then
            Error
              (Diag.v ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name
                 "a :copy query must not return rows (drop the RETURNING clause)")
          else Ok ()
        in
        let* t = copy_target q in
        Ok (Some t)
    | _ -> Ok None
  in
  let row_type =
    match q.Parse.cardinality with
    | Parse.Exec | Parse.Copy -> None
    | _ -> Some (row_type_name cfg d)
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
  Ok { d; row_type; row_origin; cols; type_fields; ps; copy }

(* ---------- collecting shared pieces ---------- *)

let collect_enums resolved =
  let tbl = Hashtbl.create 8 in
  let conflict = ref None in
  let bad_name = ref None in
  let rec add ~ctx = function
    | Typemap.Enum (n, labels) -> (
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
    | Typemap.Option t | Typemap.Array (t, _) | Typemap.Range t | Typemap.Multirange t ->
        add ~ctx t
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
            | Some (prev, prev_origin) when prev = r.type_fields && prev_origin = origin
              ->
                go tl
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

let collect_composites cfg described =
  let seen = Hashtbl.create 16 and result = ref [] in
  let rec visit pg =
    match map_type cfg ~key:None pg ~nullable:false with
    | None ->
        Diag.error "unsupported nested PostgreSQL type %s"
          (Pg_type.qualified pg.Pg_type.id)
    | Some (Typemap.Custom _) -> Ok ()
    | Some _ when Hashtbl.mem seen pg.id -> Ok ()
    | Some _ -> (
        Hashtbl.add seen pg.id ();
        let child id = visit (Pg_type.at pg id) in
        match Pg_type.kind pg with
        | Pg_type.Composite attrs ->
            let* fields =
              map_result
                (fun (a : Pg_type.attribute) ->
                  let typ = Pg_type.at pg a.typ in
                  let key = Some (Pg_type.qualified pg.id ^ "." ^ a.name) in
                  let* () =
                    match map_type cfg ~key typ ~nullable:false with
                    | Some (Typemap.Custom _) -> Ok ()
                    | _ -> visit typ
                  in
                  let fname =
                    Option.value
                      (Config.renamed cfg (Pg_type.qualified pg.id ^ "." ^ a.name))
                      ~default:a.name
                  in
                  if not (Gen_util.is_lower_ident fname) then
                    Diag.error "composite %s field %S needs a rename"
                      (Pg_type.qualified pg.id) fname
                  else
                    match map_type cfg ~key typ ~nullable:true with
                    | Some ftype -> Ok { fname; ftype; ord = a.number }
                    | None ->
                        Diag.error "composite %s field %s has unsupported type %s"
                          (Pg_type.qualified pg.id) a.name (Pg_type.qualified typ.id))
                attrs
            in
            if
              List.length fields
              <> List.length
                   (List.sort_uniq String.compare (List.map (fun f -> f.fname) fields))
            then Diag.error "duplicate composite field in %s" (Pg_type.qualified pg.id)
            else (
              result := (rename_type cfg pg.id, fields) :: !result;
              Ok ())
        | Pg_type.Array a -> child a.element
        | Pg_type.Domain d -> child d.base
        | Pg_type.Range id | Pg_type.Multirange id -> child id
        | _ -> Ok ())
  in
  let* _ =
    map_result
      (fun (d : Describe.described) ->
        let q = d.query in
        let keep key pg =
          match map_type cfg ~key pg ~nullable:false with
          | Some (Typemap.Custom _) -> None
          | _ -> Some pg
        in
        let roots =
          List.filter_map
            (fun (c : Describe.column) ->
              keep (Option.map (fun t -> t ^ "." ^ c.name) c.table) c.pg_type)
            d.columns
          @ List.filter_map
              (fun (p : Describe.param) -> keep (Some (q.name ^ "." ^ p.pname)) p.pg_type)
              d.params
        in
        Result.map_error
          (fun e ->
            { e with Diag.file = Some q.file; line = Some q.line; query = Some q.name })
          (map_result visit roots))
      described
  in
  Ok (List.sort compare !result)

let enums_in_fields fields =
  let rec walk = function
    | Typemap.Enum (n, l) -> [ (n, l) ]
    | Typemap.Option t | Typemap.Array (t, _) | Typemap.Range t | Typemap.Multirange t ->
        walk t
    | _ -> []
  in
  List.concat_map (fun f -> walk f.ftype) fields
