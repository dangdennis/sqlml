(* The oracle uses an independently prepared statement and raw catalog queries.
   It deliberately does not call Pg_type.discover to compute expected identities. *)
open Sqlml_gen

let get = function Ok x -> x | Error e -> failwith (Diag.to_string e)
let pq = function Ok x -> x | Error (e : Pq.diag) -> failwith e.message
let check label b = if not b then failwith label
let exec conn sql = Pq.with_result (pq (Pq.check (Pq.exec conn sql))) (fun _ -> ())
let json_string s = `String s
let write path text = Out_channel.with_open_bin path (fun c -> output_string c text)

let schema =
  {|
CREATE SCHEMA compiler_a;
CREATE SCHEMA compiler_b;
CREATE TYPE compiler_a.status AS ENUM ('open','closed');
CREATE TYPE compiler_b.status AS ENUM ('open','archived');
CREATE DOMAIN compiler_a.positive AS bigint CHECK (VALUE>0);
CREATE DOMAIN compiler_a.positive_list AS compiler_a.positive[];
CREATE TYPE compiler_a.payload AS (label text,states compiler_a.status[],amount compiler_a.positive,span int8range);
CREATE TYPE compiler_b.payload AS (label text,state compiler_b.status);
CREATE TYPE compiler_a.text_range AS RANGE (subtype=text,collation="C");
CREATE TABLE compiler_a.fixture (id int PRIMARY KEY, value compiler_a.payload);
INSERT INTO compiler_a.fixture VALUES (1,ROW('x',ARRAY['open'::compiler_a.status],1,'[1,3)'::int8range));
CREATE FUNCTION compiler_a.strict_value(int) RETURNS int LANGUAGE sql IMMUTABLE STRICT AS 'SELECT $1';
CREATE FUNCTION compiler_a.null_value(int) RETURNS int LANGUAGE sql IMMUTABLE AS 'SELECT NULL::int';
CREATE TYPE compiler_a.dropped AS (old int, kept text);
ALTER TYPE compiler_a.dropped DROP ATTRIBUTE old;
|}

let expressions =
  [|
    "NULL::int4";
    "2147483647::int4";
    "(-9223372036854775808)::int8";
    "9223372036854775807::int8";
    "'comma,quote\"'::text";
    "true";
    "'1.23456789'::numeric(20,8)";
    "'abc'::varchar(12)";
    "ARRAY[1,NULL,3]::int4[]";
    "'[0:1][4:5]={{1,NULL},{3,4}}'::int8[]";
    "ARRAY[]::text[]";
    "'open'::compiler_a.status";
    "'archived'::compiler_b.status";
    "1::compiler_a.positive";
    "ARRAY[1,2]::compiler_a.positive_list";
    "ROW('x',ARRAY['open'::compiler_a.status,NULL],2,'[1,3)'::int8range)::compiler_a.payload";
    "ROW(NULL,NULL,NULL,NULL)::compiler_a.payload";
    "NULL::compiler_a.payload";
    "ARRAY[NULL::compiler_a.payload]";
    "ROW('', 'archived')::compiler_b.payload";
    "'[1,9)'::int8range";
    "'empty'::int4range";
    "'{[1,3),[8,10)}'::int8multirange";
    "'[a,z)'::compiler_a.text_range";
    "ROW('kept')::compiler_a.dropped";
    "compiler_a.strict_value(NULL)";
    "compiler_a.null_value(1)";
    "coalesce(NULL::text,'fallback')";
    "greatest(NULL,1::bigint)";
    "'{\"x\":[1,null]}'::jsonb";
    "'2024-02-29'::date";
    "ARRAY[ARRAY[1,2],ARRAY[3,4]]";
  |]

let wrap shape expression i =
  let base =
    Printf.sprintf "SELECT %s AS value, :p::int4 AS param_value, %d::int4 AS case_id"
      expression i
  in
  match shape with
  | 0 -> base
  | 1 -> "WITH q AS (" ^ base ^ ") SELECT * FROM q"
  | 2 -> "WITH q AS MATERIALIZED (" ^ base ^ ") SELECT q.* FROM q"
  | 3 -> "SELECT q.* FROM (" ^ base ^ ") q INNER JOIN (VALUES(1)) x(k) ON true"
  | 4 -> "SELECT q.* FROM (VALUES(1)) x(k) LEFT JOIN (" ^ base ^ ") q ON false"
  | 5 -> "SELECT q.* FROM (" ^ base ^ ") q RIGHT JOIN (VALUES(1)) x(k) ON false"
  | 6 -> "SELECT q.* FROM (" ^ base ^ ") q FULL JOIN (VALUES(1)) x(k) ON false"
  | 7 -> "SELECT q.* FROM (VALUES(1)) x(k) CROSS JOIN LATERAL (" ^ base ^ ") q"
  | 8 -> "SELECT * FROM (" ^ base ^ ") q UNION ALL SELECT * FROM (" ^ base ^ ") r"
  | 9 -> "SELECT q.*, row_number() OVER () AS ordinal FROM (" ^ base ^ ") q"
  | 10 ->
      "WITH RECURSIVE r(n) AS (VALUES(1) UNION ALL SELECT n+1 FROM r WHERE n<2) SELECT \
       q.* FROM r CROSS JOIN (" ^ base ^ ") q"
  | 11 ->
      "SELECT q.* FROM (" ^ base
      ^ ") q WHERE EXISTS (SELECT FROM (VALUES(1)) x(k) WHERE k=1)"
  | 12 -> "SELECT q.* FROM (" ^ base ^ ") q JOIN (" ^ base ^ ") r ON q.case_id=r.case_id"
  | 13 -> "SELECT (SELECT value FROM (" ^ base ^ ") q) AS value"
  | _ -> "SELECT q.*, count(*) OVER () AS total FROM (" ^ base ^ ") q"

let raw_identity conn oid =
  match
    pq
      (Pq.query conn
         (Printf.sprintf
            "SELECT n.nspname,t.typname FROM pg_catalog.pg_namespace \
             n,pg_catalog.pg_type t WHERE n.oid=t.typnamespace AND t.oid=%d"
            oid))
  with
  | [ r ] -> { Pg_type.schema = r.(0); name = r.(1) }
  | _ -> failwith "oracle missing type"

let oracle_kind conn oid =
  let rows =
    pq
      (Pq.query conn
         (Printf.sprintf "SELECT typtype FROM pg_catalog.pg_type WHERE oid=%d" oid))
  in
  match rows with [ r ] -> r.(0) | _ -> failwith "oracle missing kind"

let verify_graph conn (t : Pg_type.t) =
  List.iter
    (fun (id, k) ->
      let literal s = "'" ^ String.concat "''" (String.split_on_char '\'' s) ^ "'" in
      let rows =
        pq
          (Pq.query conn
             (Printf.sprintf
                "SELECT \
                 t.oid,t.typbasetype,t.typnotnull,t.typtypmod,t.typelem,t.typrelid FROM \
                 pg_catalog.pg_type t JOIN pg_catalog.pg_namespace n ON \
                 n.oid=t.typnamespace WHERE n.nspname=%s AND t.typname=%s"
                (literal id.Pg_type.schema) (literal id.name)))
      in
      let r = match rows with [ r ] -> r | _ -> failwith "oracle graph identity" in
      let oid = int_of_string r.(0) in
      let sql q = pq (Pq.query conn (Printf.sprintf q oid)) in
      match k with
      | Pg_type.Base -> check "oracle base kind" (oracle_kind conn oid = "b")
      | Pg_type.Enum labels ->
          check "oracle enum labels/order"
            (List.map
               (fun r -> r.(0))
               (sql
                  "SELECT enumlabel FROM pg_catalog.pg_enum WHERE enumtypid=%d ORDER BY \
                   enumsortorder")
            = labels)
      | Pg_type.Domain d ->
          check "oracle domain base" (raw_identity conn (int_of_string r.(1)) = d.base);
          check "oracle domain not-null" (r.(2) = "t" = d.not_null);
          check "oracle domain typmod" (int_of_string r.(3) = d.typmod);
          check "oracle domain constraints"
            (List.map
               (fun r -> r.(0))
               (sql
                  "SELECT pg_catalog.pg_get_constraintdef(oid) FROM \
                   pg_catalog.pg_constraint WHERE contypid=%d ORDER BY conname")
            = d.constraints)
      | Pg_type.Array a ->
          check "oracle array element"
            (raw_identity conn (int_of_string r.(4)) = a.element);
          let delim =
            pq
              (Pq.query conn
                 (Printf.sprintf "SELECT typdelim FROM pg_catalog.pg_type WHERE oid=%s"
                    r.(4)))
          in
          check "oracle delimiter" ((List.hd delim).(0) = String.make 1 a.delimiter)
      | Pg_type.Composite attrs ->
          let rows =
            pq
              (Pq.query conn
                 (Printf.sprintf
                    "SELECT attname,atttypid,attnum,atttypmod FROM \
                     pg_catalog.pg_attribute WHERE attrelid=%s AND attnum>0 AND NOT \
                     attisdropped ORDER BY attnum"
                    r.(5)))
          in
          check "oracle composite arity" (List.length rows = List.length attrs);
          List.iter2
            (fun r (a : Pg_type.attribute) ->
              check "oracle composite field"
                (r.(0) = a.name
                && raw_identity conn (int_of_string r.(1)) = a.typ
                && int_of_string r.(2) = a.number
                && int_of_string r.(3) = a.typmod))
            rows attrs
      | Pg_type.Range sub ->
          let rs = sql "SELECT rngsubtype FROM pg_catalog.pg_range WHERE rngtypid=%d" in
          check "oracle range subtype"
            (raw_identity conn (int_of_string (List.hd rs).(0)) = sub)
      | Pg_type.Multirange range ->
          let rs =
            sql "SELECT rngtypid FROM pg_catalog.pg_range WHERE rngmultitypid=%d"
          in
          check "oracle multirange range"
            (raw_identity conn (int_of_string (List.hd rs).(0)) = range)
      | Pg_type.Unsupported _ -> failwith "unexpected unsupported corpus type")
    t.registry

let case conn ~seed ~output ~checks i =
  let expr = expressions.((i + seed) mod Array.length expressions) in
  let sql = wrap (i / Array.length expressions mod 15) expr i in
  let name = Printf.sprintf "Q%d" i in
  let q =
    match
      get (Parse.of_string ~file:(name ^ ".sql") ("-- name: " ^ name ^ " :many\n" ^ sql))
    with
    | [ q ] -> q
    | _ -> assert false
  in
  exec conn "DEALLOCATE ALL";
  let d = List.hd (get (Describe.describe_all conn [ q ])) in
  ignore (get (Emit.generate ~src:"corpus" [ d ]));
  (match d.columns with c :: _ -> verify_graph conn c.pg_type | [] -> ());
  Pq.with_result (pq (Pq.check (Pq.prepare conn "oracle" q.sql))) (fun _ -> ());
  Pq.with_result
    (pq (Pq.check (Pq.describe_prepared conn "oracle")))
    (fun r ->
      check "column count" (Pq.nfields r = List.length d.columns);
      check "parameter count" (Pq.nparams r = List.length d.params);
      List.iteri
        (fun j (p : Describe.param) ->
          check "parameter identity" (raw_identity conn (Pq.paramtype r j) = p.pg_type.id))
        d.params;
      List.iteri
        (fun j (c : Describe.column) ->
          check "result name" (Pq.fname r j = c.name);
          check "result identity" (raw_identity conn (Pq.ftype r j) = c.pg_type.id);
          check "type modifier" (Pq.fmod r j = c.typmod);
          check "conservative nullability" c.nullable;
          let tag = oracle_kind conn (Pq.ftype r j) in
          check "type classification"
            (match (Pg_type.kind c.pg_type, tag) with
            | Pg_type.Base, "b"
            | Pg_type.Array _, "b"
            | Pg_type.Enum _, "e"
            | Pg_type.Domain _, "d"
            | Pg_type.Composite _, "c"
            | Pg_type.Range _, "r"
            | Pg_type.Multirange _, "m" ->
                true
            | _ -> false))
        d.columns);
  Pq.with_result
    (pq
       (Pq.check
          (Pq.exec_params conn q.sql (Array.make (List.length q.params) (Some "1")))))
    (fun r ->
      check "execution type shape" (Pq.nfields r = List.length d.columns);
      for row = 0 to Pq.ntuples r - 1 do
        let values =
          List.init (Pq.nfields r) (fun j ->
              if Pq.getisnull r row j then "Sqlml.Value.Null"
              else Printf.sprintf "Sqlml.Value.Text %S" (Pq.getvalue r row j))
        in
        Printf.bprintf checks "let () = ignore (Corpus.%s.decode [|%s|])\n" q.module_name
          (String.concat ";" values)
      done;
      List.iteri
        (fun j (c : Describe.column) ->
          for row = 0 to Pq.ntuples r - 1 do
            if Pq.getisnull r row j then check "NULL admitted by contract" c.nullable
          done)
        d.columns);
  let contract =
    `Assoc
      [
        ("name", json_string name);
        ("sql", json_string sql);
        ( "minimal_sql",
          json_string
            (if i / Array.length expressions mod 15 = 13 then
               "SELECT " ^ expr ^ " AS value WHERE :p::int4 IS NOT NULL"
             else wrap 0 expr i) );
        ( "params",
          `List
            (List.map
               (fun (p : Describe.param) ->
                 `Assoc
                   [ ("name", json_string p.pname); ("type", Pg_type.to_json p.pg_type) ])
               d.params) );
        ( "columns",
          `List
            (List.map
               (fun (c : Describe.column) ->
                 `Assoc
                   [
                     ("name", json_string c.name);
                     ("type", Pg_type.to_json c.pg_type);
                     ("nullable", `Bool c.nullable);
                   ])
               d.columns) );
      ]
  in
  output_string output (Yojson.Safe.to_string contract ^ "\n");
  d

let regressions conn out =
  let parse sql =
    match get (Parse.of_string ~file:"regression.sql" sql) with
    | [ q ] -> q
    | _ -> assert false
  in
  exec conn "DEALLOCATE ALL";
  let variant =
    parse "-- name: Drift :many\nSELECT (:x /*? + :b::bigint */)::numeric AS value"
  in
  check "dynamic parameter drift rejected"
    (Result.is_error (Describe.describe_all conn [ variant ]));
  exec conn "DEALLOCATE ALL";
  let vector = parse "-- name: Vector :many\nSELECT '1 2'::int2vector AS value" in
  check "vector is not an ordinary array"
    (Result.is_error
       (Emit.generate ~src:"regression" (get (Describe.describe_all conn [ vector ]))));
  exec conn "DEALLOCATE ALL";
  let record = parse "-- name: Anonymous :many\nSELECT ROW(1,'x') AS value" in
  check "anonymous record diagnostic"
    (Result.is_error
       (Emit.generate ~src:"regression" (get (Describe.describe_all conn [ record ]))));
  exec conn "DEALLOCATE ALL";
  let query =
    parse
      "-- name: Snapshot :many\n\
       SELECT 'open'::compiler_a.status AS a, 'archived'::compiler_b.status AS b"
  in
  let described = get (Describe.describe_all conn [ query ]) in
  ignore (get (Snapshot.write ~queries_dir:out ~config:Config.empty described));
  let offline =
    get (Snapshot.describe_offline ~queries_dir:out ~config:Config.empty [ query ])
  in
  check "snapshot graph and output equivalence"
    (Emit.generate ~src:"regression" described = Emit.generate ~src:"regression" offline);
  write
    (Filename.concat out "fingerprint.json")
    (Yojson.Safe.to_string
       (`List
          (List.map
             (fun (d : Describe.described) ->
               `List
                 (List.map
                    (fun (c : Describe.column) -> Pg_type.to_json c.pg_type)
                    d.columns))
             described)))

let () =
  let described = ref [] and checks = Buffer.create 4096 in
  let single = ref (-1) in
  let count = ref 1000 and seed = ref 0 and out = ref "/tmp/sqlml-inference" in
  Arg.parse
    [
      ("--case", Arg.Set_int single, "replay a single case");
      ("--cases", Arg.Set_int count, "case count");
      ("--seed", Arg.Set_int seed, "replay seed");
      ("--output", Arg.Set_string out, "artifact directory");
    ]
    (fun _ -> failwith "unexpected argument")
    "inference";
  if !count < 1 || !seed < 0 then
    invalid_arg "positive case count and nonnegative seed required";
  if not (Sys.file_exists !out) then Unix.mkdir !out 0o755;
  write (Filename.concat !out "schema.sql") schema;
  let conn = get (Describe.connect (Pq.conninfo_of_env ())) in
  Fun.protect
    ~finally:(fun () -> Pq.finish conn)
    (fun () ->
      exec conn "BEGIN";
      exec conn "SET LOCAL statement_timeout='5s'";
      exec conn schema;
      Fun.protect
        ~finally:(fun () -> exec conn "ROLLBACK")
        (fun () ->
          let version = List.hd (pq (Pq.query conn "SHOW server_version")) in
          write
            (Filename.concat !out "run.json")
            (Yojson.Safe.pretty_to_string
               (`Assoc
                  [
                    ("cases", `Int (if !single < 0 then !count else 1));
                    ("seed", `Int !seed);
                    ("postgres", json_string version.(0));
                    ("pgenie", json_string "0.15.0");
                  ]));
          regressions conn !out;
          Out_channel.with_open_bin (Filename.concat !out "contracts.jsonl")
            (fun output ->
              for
                i = if !single < 0 then 0 else !single
                to if !single < 0 then !count - 1 else !single
              do
                try described := case conn ~seed:!seed ~output ~checks i :: !described
                with exn ->
                  write
                    (Filename.concat !out "failure.txt")
                    (Printf.sprintf "seed=%d case=%d\n%s\n%s" !seed i
                       (wrap
                          (i / Array.length expressions mod 15)
                          expressions.((i + !seed) mod Array.length expressions)
                          i)
                       (Printexc.to_string exn));
                  raise exn
              done)));
  let mli, ml = get (Emit.generate ~src:"corpus" (List.rev !described)) in
  write (Filename.concat !out "corpus.mli") mli;
  write (Filename.concat !out "corpus.ml") ml;
  write
    (Filename.concat !out "decode_checks.ml")
    (Buffer.contents checks
   ^ "let () = print_endline \"compiled corpus decoders: all good\"\n");
  Printf.printf "inference: %d cases passed (seed %d)\n"
    (if !single < 0 then !count else 1)
    !seed
