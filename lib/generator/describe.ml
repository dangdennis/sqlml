(* See describe.mli. PQprepare uses nParams = 0 so the server infers parameter
   types -- the inference is the answer we want -- and PQdescribePrepared
   executes nothing. *)

type override =
  | No_override
  | Force_not_null (* trailing ! on the column alias *)
  | Force_nullable (* trailing ? *)

type column = {
  name : string; (* alias with any !/? stripped *)
  pg_type : Pg_type.t;
  typmod : int;
  table : string option; (* relname, when the column comes from a table *)
  table_oid : int; (* 0 when not a plain column reference *)
  table_col : int; (* attnum; 0 when table_oid is 0 *)
  nullable : bool;
}

type param = {
  index : int;
  pname : string;
  pg_type : Pg_type.t;
  pnullable : bool; (* from a trailing ? on the placeholder; see Parse.param *)
}

type described = {
  query : Parse.t;
  params : param list;
  columns : column list;
  (* Set when the result is exactly one table's full column set, which is what
       licenses a shared model type rather than a per-query row type. *)
  model_table : string option;
}

let ( let* ) = Result.bind

(* ---------- connection ---------- *)

let conninfo_of_env = Pq.conninfo_of_env

let connect conninfo =
  let c = Pq.connect conninfo in
  if Pq.connect_ok c then Ok c
  else begin
    let m = String.trim (Pq.error_message c) in
    Pq.finish c;
    Diag.error "could not connect: %s" m
  end

(* ---------- name overrides ---------- *)

(* SELECT coalesce(x,0) AS "total!" -- the marker rides through Postgres inside
   a quoted alias, so we never rewrite the user's SELECT list and a marker can
   never collide with SQL syntax. *)
let split_override name =
  let n = String.length name in
  if n >= 2 && name.[n - 1] = '!' then (String.sub name 0 (n - 1), Force_not_null)
  else if n >= 2 && name.[n - 1] = '?' then (String.sub name 0 (n - 1), Force_nullable)
  else (name, No_override)

(* ---------- catalog ---------- *)

let quote_ints xs = String.concat "," (List.map string_of_int xs)

(* table name + its live column count, for shared model detection *)
let table_info conn oids =
  if oids = [] then Ok []
  else
    let* rows =
      Pq.query conn
        (Printf.sprintf
           "select c.oid, pg_catalog.format('%%I.%%I',n.nspname,c.relname), (select \
            count(*) from pg_attribute a where a.attrelid = c.oid and a.attnum > 0 and \
            not a.attisdropped) from pg_class c join pg_namespace n on \
            n.oid=c.relnamespace where c.oid in (%s)"
           (quote_ints oids))
    in
    Ok (List.map (fun r -> (int_of_string r.(0), (r.(1), int_of_string r.(2)))) rows)

(* ---------- describing one query ---------- *)

type raw_col = {
  rname : string;
  roverride : override;
  rtype : int;
  rtypmod : int;
  rtable : int;
  rcol : int;
}

let msg (d : Pq.diag) = d.Pq.message

let describe_text conn ~fail ~sql ~nparams ~stmt_name =
  let pr = Pq.prepare conn stmt_name sql in
  match Pq.check pr with
  | Error e -> fail (msg e)
  | Ok pr -> (
      Pq.clear pr;
      let dr = Pq.describe_prepared conn stmt_name in
      match Pq.check dr with
      | Error e -> fail (msg e)
      | Ok dr ->
          let params = List.init (Pq.nparams dr) (fun i -> Pq.paramtype dr i) in
          let cols =
            List.init (Pq.nfields dr) (fun i ->
                let name, roverride = split_override (Pq.fname dr i) in
                {
                  rname = name;
                  roverride;
                  rtype = Pq.ftype dr i;
                  rtypmod = Pq.fmod dr i;
                  rtable = Pq.ftable dr i;
                  rcol = Pq.ftablecol dr i;
                })
          in
          Pq.clear dr;
          if List.length params <> nparams then
            fail
              (Printf.sprintf "server inferred %d parameter(s) but the SQL names %d"
                 (List.length params) nparams)
          else Ok (params, cols))

let describe_one conn (q : Parse.t) ~stmt_name =
  let fail message =
    Error (Diag.v ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name message)
  in
  describe_text conn ~fail ~sql:q.Parse.sql ~nparams:(List.length q.Parse.params)
    ~stmt_name

(* Every inclusion combination of a dynamic query is a distinct statement, and
   each one is verified against the server. The variants must also agree on the
   result shape -- an optional block that adds a SELECT column would give the
   same OCaml function different row types depending on its arguments, which is
   why that is an error rather than a feature. *)
let check_variants conn (q : Parse.t) ~stmt_base ~full_params ~full_cols =
  match q.Parse.dynamic with
  | None -> Ok ()
  | Some d ->
      let fail message =
        Error (Diag.v ~file:q.Parse.file ~line:q.Parse.line ~query:q.Parse.name message)
      in
      let shape cols =
        List.map
          (fun c -> (c.rname, c.roverride, c.rtype, c.rtypmod, c.rtable, c.rcol))
          cols
      in
      let expected = shape full_cols in
      let n = Array.length d.Parse.variant_sqls in
      let rec go mask =
        if mask >= n - 1 then Ok () (* the full variant is the canonical describe *)
        else
          let sql = d.Parse.variant_sqls.(mask) in
          let nparams = d.Parse.variant_nparams.(mask) in
          match
            describe_text conn ~fail ~sql ~nparams
              ~stmt_name:(Printf.sprintf "%s_v%d" stmt_base mask)
          with
          | Error e ->
              Error
                {
                  e with
                  Diag.message = Printf.sprintf "variant %d: %s" mask e.Diag.message;
                }
          | Ok (params, cols) ->
              let absent =
                Array.to_list d.Parse.block_params
                |> List.mapi (fun k names ->
                    if mask land (1 lsl k) = 0 then names else [])
                |> List.concat
              in
              let expected_params =
                List.combine q.Parse.params full_params
                |> List.filter_map (fun (p, oid) ->
                    if List.mem p.Parse.pname absent then None else Some oid)
              in
              if params <> expected_params then
                fail
                  (Printf.sprintf "optional blocks change parameter types in variant %d"
                     mask)
              else if shape cols <> expected then
                fail
                  (Printf.sprintf
                     "optional blocks change the result shape: variant %d returns \
                      different columns than the full query. Blocks may filter rows, not \
                      add or remove columns."
                     mask)
              else go (mask + 1)
      in
      go 0

(* ---------- driver ---------- *)

let uniq l = List.sort_uniq compare l

let describe_all conn (queries : Parse.t list) =
  (* one pass to collect raw descriptions, then batch every catalog lookup *)
  let rec collect acc i = function
    | [] -> Ok (List.rev acc)
    | q :: tl -> (
        match describe_one conn q ~stmt_name:(Printf.sprintf "sqlml_%d" i) with
        | Error e -> Error e
        | Ok (params, cols) -> (
            match
              check_variants conn q ~stmt_base:(Printf.sprintf "sqlml_%d" i)
                ~full_params:params ~full_cols:cols
            with
            | Error e -> Error e
            | Ok () -> collect ((q, params, cols) :: acc) (i + 1) tl))
  in
  let* raws = collect [] 0 queries in
  let all_type_oids =
    uniq (List.concat_map (fun (_, ps, cs) -> ps @ List.map (fun c -> c.rtype) cs) raws)
  in
  let all_pairs =
    uniq
      (List.concat_map (fun (_, _, cs) -> List.map (fun c -> (c.rtable, c.rcol)) cs) raws)
  in
  let all_tables = uniq (List.filter (fun t -> t <> 0) (List.map fst all_pairs)) in
  let catalog f = Result.map_error (fun (d : Pq.diag) -> Diag.v d.Pq.message) f in
  let* types = Pg_type.discover conn all_type_oids in
  let* tinfo = catalog (table_info conn all_tables) in
  let info oid = List.assoc oid types in
  Ok
    (List.map
       (fun (q, ps, cs) ->
         let columns =
           List.map
             (fun c ->
               let nullable =
                 match c.roverride with
                 | Force_not_null -> false
                 | Force_nullable -> true
                 | No_override -> true
               in
               {
                 name = c.rname;
                 pg_type = info c.rtype;
                 typmod = c.rtypmod;
                 table = Option.map fst (List.assoc_opt c.rtable tinfo);
                 table_oid = c.rtable;
                 table_col = c.rcol;
                 nullable;
               })
             cs
         in
         (* Shared model: every column is a plain reference to the same table,
            and together they cover all of that table's live columns. A partial
            projection keeps its own per-query row type. *)
         let model_table =
           match columns with
           | [] -> None
           | first :: _ -> (
               let t = first.table_oid in
               if t = 0 || not (List.for_all (fun c -> c.table_oid = t) columns) then None
               else
                 match List.assoc_opt t tinfo with
                 | Some (tname, live)
                   when List.length columns = live
                        && live
                           = List.length (uniq (List.map (fun c -> c.table_col) columns))
                   ->
                     Some tname
                 | _ -> None)
         in
         let params =
           List.mapi
             (fun i oid ->
               let decl =
                 List.find_opt
                   (fun (p : Parse.param) -> p.Parse.index = i + 1)
                   q.Parse.params
               in
               {
                 index = i + 1;
                 pname =
                   (match decl with
                   | Some p -> p.Parse.pname
                   | None -> Printf.sprintf "arg%d" (i + 1));
                 pg_type = info oid;
                 pnullable =
                   (match decl with Some p -> p.Parse.nullable | None -> false);
               })
             ps
         in
         { query = q; params; columns; model_table })
       raws)

(* Human-readable report for `sqlml describe`, one query per call. *)
let report d =
  let b = Buffer.create 256 in
  let bp fmt = Printf.ksprintf (Buffer.add_string b) fmt in
  let q = d.query in
  bp "%s  (:%s)  %s:%d\n" q.Parse.name
    (Parse.string_of_cardinality q.Parse.cardinality)
    q.Parse.file q.Parse.line;
  (match d.model_table with Some t -> bp "  model    : %s (full row)\n" t | None -> ());
  List.iter
    (fun p ->
      bp "  param $%d : %-14s %s%s%s\n" p.index
        (Pg_type.qualified p.pg_type.id)
        p.pname
        (if p.pnullable then "  [nullable]" else "")
        (match match Pg_type.kind p.pg_type with Pg_type.Enum l -> l | _ -> [] with
        | [] -> ""
        | l -> "  enum{" ^ String.concat "|" l ^ "}"))
    d.params;
  List.iter
    (fun c ->
      bp "  col      : %-14s %-14s %s%s%s\n" c.name
        (Pg_type.qualified c.pg_type.id)
        (if c.nullable then "nullable" else "asserted NOT NULL")
        (if c.table_oid = 0 then "  [computed]"
         else Printf.sprintf "  [tbl %d col %d]" c.table_oid c.table_col)
        (match match Pg_type.kind c.pg_type with Pg_type.Enum l -> l | _ -> [] with
        | [] -> ""
        | l -> "  enum{" ^ String.concat "|" l ^ "}"))
    d.columns;
  Buffer.add_char b '\n';
  Buffer.contents b

(* Explicit constructors: the records are private so pipeline code cannot
   fabricate them, but two legitimate producers exist besides the server --
   tests, and the offline snapshot cache, which deserializes exactly these. *)
let column ~name ~pg_type ~typmod ~table ~table_oid ~table_col ~nullable =
  { name; pg_type; typmod; table; table_oid; table_col; nullable }

let param ~index ~pname ~pg_type ~pnullable = { index; pname; pg_type; pnullable }

let v_column ~name ~type_name ~elem_type_name ~table ~table_oid ~table_col ~nullable
    ~enum_labels =
  column ~name
    ~pg_type:(Pg_type.legacy ~name:type_name ~element:elem_type_name ~labels:enum_labels)
    ~typmod:(-1) ~table ~table_oid ~table_col ~nullable

let v_param ~index ~pname ~ptype_name ~pelem_type_name ~penum_labels ~pnullable =
  param ~index ~pname
    ~pg_type:
      (Pg_type.legacy ~name:ptype_name ~element:pelem_type_name ~labels:penum_labels)
    ~pnullable

let v_described ~query ~params ~columns ~model_table =
  { query; params; columns; model_table }
