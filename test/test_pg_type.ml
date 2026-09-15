open Sqlml_gen

let check label condition = if not condition then failwith label
let id schema name : Pg_type.id = { schema; name }

let q =
  match Parse.of_string ~file:"graph.sql" "-- name: Graph :many\nSELECT 1" with
  | Ok [ q ] -> q
  | _ -> assert false

let describe columns = Describe.v_described ~query:q ~params:[] ~columns ~model_table:None

let column name typ =
  Describe.column ~name ~pg_type:typ ~typmod:(-1) ~table:None ~table_oid:0 ~table_col:0
    ~nullable:true

let () =
  let a = id "a" "status" and b = id "b" "status" in
  let registry = [ (a, Pg_type.Enum [ "open" ]); (b, Pg_type.Enum [ "open" ]) ] in
  let ta : Pg_type.t = { id = a; registry } and tb : Pg_type.t = { id = b; registry } in
  check "schema identity survives JSON" (Pg_type.of_json (Pg_type.to_json ta) = ta);
  check "same labels do not merge identities"
    (Typemap.of_type ta ~nullable:false <> Typemap.of_type tb ~nullable:false);
  check "schema enums emit together"
    (Result.is_ok
       (Emit.generate ~src:"test" [ describe [ column "a" ta; column "b" tb ] ]));
  let d = id "a" "amount" and base = id "pg_catalog" "int8" in
  let domain : Pg_type.t =
    {
      id = d;
      registry =
        [
          ( d,
            Pg_type.Domain
              {
                base;
                not_null = true;
                typmod = -1;
                constraints = [ "CHECK (VALUE > 0)" ];
              } );
          (base, Pg_type.Base);
        ];
    }
  in
  check "domain maps to nullable base"
    (Typemap.of_type domain ~nullable:true = Some (Typemap.Option Typemap.Int64));
  let recursive = id "a" "recursive" in
  let t : Pg_type.t =
    {
      id = recursive;
      registry =
        [
          ( recursive,
            Pg_type.Composite
              [ { name = "next"; typ = recursive; number = 1; typmod = -1 } ] );
        ];
    }
  in
  check "recursive graph JSON" (Pg_type.of_json (Pg_type.to_json t) = t);
  check "recursive declaration terminates"
    (Result.is_ok (Emit.generate ~src:"test" [ describe [ column "node" t ] ]));
  let broken : Pg_type.t = { id = d; registry = [ (d, Pg_type.Range base) ] } in
  check "dangling graph rejected"
    (try
       ignore (Pg_type.of_json (Pg_type.to_json broken));
       false
     with Invalid_argument _ -> true);
  let empty_id = id "a" "empty" in
  let empty : Pg_type.t = { id = empty_id; registry = [ (empty_id, Pg_type.Enum []) ] } in
  check "empty enum emits"
    (Result.is_ok (Emit.generate ~src:"test" [ describe [ column "empty" empty ] ]));
  let collision : Pg_type.t =
    { id = a; registry = [ (a, Pg_type.Enum [ "a-b"; "a_b" ]) ] }
  in
  check "enum constructor collision rejected"
    (Result.is_error (Emit.generate ~src:"test" [ describe [ column "bad" collision ] ]));
  check "quoted identity is unambiguous"
    (Pg_type.qualified (id "a.b" "c") <> Pg_type.qualified (id "a" "b.c"));
  let anonymous : Pg_type.t =
    {
      id = id "pg_catalog" "record";
      registry = [ (id "pg_catalog" "record", Pg_type.Unsupported "p") ];
    }
  in
  check "anonymous record rejected"
    (Result.is_error
       (Emit.generate ~src:"test" [ describe [ column "anonymous" anonymous ] ]));
  print_endline "type graph: all good"
