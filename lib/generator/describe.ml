(** Asking PostgreSQL what a query's types are.

   For each parsed query we PQprepare it (nParams = 0, so the server infers
   parameter types -- the inference is the answer we want) and then
   PQdescribePrepared. That yields, without executing anything:

     - parameter type OIDs
     - per result column: name, type OID, and crucially the originating
       table OID + column number, or 0/0 when the column is an expression

   Everything else is catalog lookups over those OIDs. *)

type override =
  | No_override
  | Force_not_null (* trailing ! on the column alias *)
  | Force_nullable (* trailing ? *)

type column =
  { name : string (* alias with any !/? stripped *)
  ; type_oid : int
  ; type_name : string
  ; elem_type_name : string option (* Some when the type is an array *)
  ; table_oid : int (* 0 when not a plain column reference *)
  ; table_col : int (* attnum; 0 when table_oid is 0 *)
  ; nullable : bool
  ; enum_labels : string list (* non-empty when the type is an enum *)
  }

type param =
  { index : int
  ; pname : string
  ; ptype_oid : int
  ; ptype_name : string
  ; pelem_type_name : string option
  ; penum_labels : string list
  ; pnullable : bool (* from a trailing ? on the placeholder; see Parse.param *)
  }

type described =
  { query : Parse.t
  ; params : param list
  ; columns : column list
  ; (* Set when the result is exactly one table's full column set, which is what
       licenses a shared model type rather than a per-query row type. *)
    model_table : string option
  }

type error =
  { file : string
  ; line : int
  ; qname : string
  ; message : string
  }

let ( let* ) = Result.bind

(* ---------- connection ---------- *)

let conninfo_of_env () =
  match Sys.getenv_opt "DATABASE_URL" with
  | Some u when String.trim u <> "" -> u
  | _ ->
    let get k d = match Sys.getenv_opt k with Some v when v <> "" -> v | _ -> d in
    Printf.sprintf "host=%s port=%s user=%s dbname=%s%s" (get "PGHOST" "127.0.0.1")
      (get "PGPORT" "5432") (get "PGUSER" "postgres") (get "PGDATABASE" "postgres")
      (match Sys.getenv_opt "PGPASSWORD" with Some p when p <> "" -> " password=" ^ p | _ -> "")

let connect conninfo =
  let c = Pq.connect conninfo in
  if Pq.connect_ok c then Ok c
  else (
    let m = String.trim (Pq.error_message c) in
    Pq.finish c;
    Error m)

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

(* name, element type name (arrays only), and element oid, in one pass. An array
   type in Postgres has typcategory 'A' and a typelem pointing at its element
   type -- text[] is a distinct type named _text whose typelem is text. *)
type type_info =
  { tname : string
  ; telem_name : string option
  ; telem_oid : int
  }

let type_infos conn oids =
  if oids = [] then Ok []
  else
    let* rows =
      Pq.query conn
        (Printf.sprintf
           "select t.oid, t.typname, t.typcategory, coalesce(e.typname, ''), \
            coalesce(t.typelem, 0) from pg_type t left join pg_type e on e.oid = t.typelem \
            where t.oid in (%s)"
           (quote_ints oids))
    in
    Ok
      (List.map
         (fun r ->
           let is_array = r.(2) = "A" && r.(4) <> "0" in
           ( int_of_string r.(0)
           , { tname = r.(1)
             ; telem_name = (if is_array then Some r.(3) else None)
             ; telem_oid = (if is_array then int_of_string r.(4) else 0)
             } ))
         rows)

let enum_labels conn oids =
  if oids = [] then Ok []
  else
    let* rows =
      Pq.query conn
        (Printf.sprintf
           "select enumtypid, enumlabel from pg_enum where enumtypid in (%s) order by \
            enumtypid, enumsortorder"
           (quote_ints oids))
    in
    let tbl = Hashtbl.create 8 in
    List.iter
      (fun r ->
        let k = int_of_string r.(0) in
        Hashtbl.replace tbl k (Hashtbl.find_opt tbl k |> Option.value ~default:[] |> fun l -> r.(1) :: l))
      rows;
    Ok (Hashtbl.fold (fun k v acc -> (k, List.rev v) :: acc) tbl [])

(* attnotnull for every (table, column) pair we saw *)
let not_null_map conn pairs =
  let pairs = List.filter (fun (t, c) -> t <> 0 && c > 0) pairs in
  if pairs = [] then Ok []
  else
    let clause =
      pairs
      |> List.map (fun (t, c) -> Printf.sprintf "(%d,%d)" t c)
      |> String.concat ","
    in
    let* rows =
      Pq.query conn
        (Printf.sprintf
           "select attrelid, attnum, attnotnull from pg_attribute where (attrelid, attnum) in \
            (%s)"
           clause)
    in
    Ok
      (List.map
         (fun r -> ((int_of_string r.(0), int_of_string r.(1)), r.(2) = "t"))
         rows)

(* table name + its live column count, for shared model detection *)
let table_info conn oids =
  if oids = [] then Ok []
  else
    let* rows =
      Pq.query conn
        (Printf.sprintf
           "select c.oid, c.relname, (select count(*) from pg_attribute a where a.attrelid = \
            c.oid and a.attnum > 0 and not a.attisdropped) from pg_class c where c.oid in (%s)"
           (quote_ints oids))
    in
    Ok (List.map (fun r -> (int_of_string r.(0), (r.(1), int_of_string r.(2)))) rows)

(* ---------- describing one query ---------- *)

type raw_col =
  { rname : string
  ; roverride : override
  ; rtype : int
  ; rtable : int
  ; rcol : int
  }

let msg (d : Pq.diag) = d.Pq.message

let describe_one conn (q : Parse.t) ~stmt_name =
  let fail message = Error { file = q.Parse.file; line = q.Parse.line; qname = q.Parse.name; message } in
  let pr = Pq.prepare conn stmt_name q.Parse.sql in
  match Pq.check pr with
  | Error e -> fail (msg e)
  | Ok pr ->
    Pq.clear pr;
    let dr = Pq.describe_prepared conn stmt_name in
    (match Pq.check dr with
     | Error e -> fail (msg e)
     | Ok dr ->
       let params = List.init (Pq.nparams dr) (fun i -> Pq.paramtype dr i) in
       let cols =
         List.init (Pq.nfields dr) (fun i ->
             let name, roverride = split_override (Pq.fname dr i) in
             { rname = name
             ; roverride
             ; rtype = Pq.ftype dr i
             ; rtable = Pq.ftable dr i
             ; rcol = Pq.ftablecol dr i
             })
       in
       Pq.clear dr;
       if List.length params <> List.length q.Parse.params then
         fail
           (Printf.sprintf "server inferred %d parameter(s) but the SQL names %d"
              (List.length params) (List.length q.Parse.params))
       else Ok (params, cols))

(* ---------- driver ---------- *)

let uniq l = List.sort_uniq compare l

let describe_all conn (queries : Parse.t list) =
  (* one pass to collect raw descriptions, then batch every catalog lookup *)
  let rec collect acc i = function
    | [] -> Ok (List.rev acc)
    | q :: tl -> (
      match describe_one conn q ~stmt_name:(Printf.sprintf "sqlml_%d" i) with
      | Error e -> Error e
      | Ok (params, cols) -> collect ((q, params, cols) :: acc) (i + 1) tl)
  in
  let* raws = collect [] 0 queries in
  let all_type_oids =
    uniq
      (List.concat_map
         (fun (_, ps, cs) -> ps @ List.map (fun c -> c.rtype) cs)
         raws)
  in
  let all_pairs = uniq (List.concat_map (fun (_, _, cs) -> List.map (fun c -> (c.rtable, c.rcol)) cs) raws) in
  let all_tables = uniq (List.filter (fun t -> t <> 0) (List.map fst all_pairs)) in
  let catalog f = Result.map_error (fun m -> { file = ""; line = 0; qname = ""; message = m }) f in
  let* tinfos = catalog (type_infos conn all_type_oids) in
  (* an array of an enum carries its labels on the element type *)
  let elem_oids = List.filter_map (fun (_, i) -> if i.telem_oid <> 0 then Some i.telem_oid else None) tinfos in
  let* elabels = catalog (enum_labels conn (uniq (all_type_oids @ elem_oids))) in
  let* nn = catalog (not_null_map conn all_pairs) in
  let* tinfo = catalog (table_info conn all_tables) in
  let info oid =
    List.assoc_opt oid tinfos |> Option.value ~default:{ tname = "unknown"; telem_name = None; telem_oid = 0 }
  in
  let type_name oid = (info oid).tname in
  let elem_name oid = (info oid).telem_name in
  (* for an array, the interesting labels are the element type's *)
  let labels oid =
    let i = info oid in
    let key = if i.telem_oid <> 0 then i.telem_oid else oid in
    List.assoc_opt key elabels |> Option.value ~default:[]
  in
  let attnotnull t c = List.assoc_opt (t, c) nn |> Option.value ~default:false in
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
                 | No_override -> not (attnotnull c.rtable c.rcol)
               in
               { name = c.rname
               ; type_oid = c.rtype
               ; type_name = type_name c.rtype
               ; elem_type_name = elem_name c.rtype
               ; table_oid = c.rtable
               ; table_col = c.rcol
               ; nullable
               ; enum_labels = labels c.rtype
               })
             cs
         in
         (* Shared model: every column is a plain reference to the same table,
            and together they cover all of that table's live columns. A partial
            projection keeps its own per-query row type. *)
         let model_table =
           match columns with
           | [] -> None
           | first :: _ ->
             let t = first.table_oid in
             if t = 0 || not (List.for_all (fun c -> c.table_oid = t) columns) then None
             else (
               match List.assoc_opt t tinfo with
               | Some (tname, live) when live = List.length (uniq (List.map (fun c -> c.table_col) columns)) ->
                 Some tname
               | _ -> None)
         in
         let params =
           List.mapi
             (fun i oid ->
               let decl =
                 List.find_opt (fun (p : Parse.param) -> p.Parse.index = i + 1) q.Parse.params
               in
               { index = i + 1
               ; pname =
                   (match decl with
                    | Some p -> p.Parse.pname
                    | None -> Printf.sprintf "arg%d" (i + 1))
               ; ptype_oid = oid
               ; ptype_name = type_name oid
               ; pelem_type_name = elem_name oid
               ; penum_labels = labels oid
               ; pnullable = (match decl with Some p -> p.Parse.nullable | None -> false)
               })
             ps
         in
         { query = q; params; columns; model_table })
       raws)

let string_of_error e =
  if e.file = "" then e.message
  else Printf.sprintf "%s:%d: %s: %s" e.file e.line e.qname e.message
