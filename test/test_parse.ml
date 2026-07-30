(* The parser is the most intricate string code in the project and used to be
   exercised only through a live-database generate. These run in `dune test`
   with nothing else on. *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

open Sqlml_gen

let parse s =
  match Parse.of_string ~file:"t.sql" s with
  | Ok [ q ] -> q
  | Ok qs -> failwith (Printf.sprintf "expected 1 query, got %d" (List.length qs))
  | Error (e : Parse.error) -> failwith e.Parse.message

let fails s = match Parse.of_string ~file:"t.sql" s with Ok _ -> false | Error _ -> true
let names (q : Parse.t) = List.map (fun (p : Parse.param) -> p.Parse.pname) q.Parse.params

let () =
  (* ---------- headers and naming ---------- *)
  let q = parse "-- name: GetUserV2 :one\nSELECT 1" in
  check "cardinality one" (q.Parse.cardinality = Parse.One);
  check "module name" (q.Parse.module_name = "Get_user_v2");
  check "one!" ((parse "-- name: X :one!\nSELECT 1").Parse.cardinality = Parse.One_strict);
  check "many" ((parse "-- name: X :many\nSELECT 1").Parse.cardinality = Parse.Many);
  check "exec" ((parse "-- name: X :exec\nSELECT 1").Parse.cardinality = Parse.Exec);
  check "unknown cardinality rejected" (fails "-- name: X :all\nSELECT 1");
  check "empty sql rejected" (fails "-- name: X :one\n");

  let q = parse "-- name: X :one\n-- First line.\n-- Second.\nSELECT 1" in
  check "docstring" (q.Parse.doc = [ "First line."; "Second." ]);

  (* ---------- named parameters ---------- *)
  let q = parse "-- name: X :one\nSELECT * FROM t WHERE a = :a AND b = :b AND a2 = :a" in
  check "params numbered by first occurrence" (names q = [ "a"; "b" ]);
  check "repeated param reuses $1"
    (q.Parse.sql = "SELECT * FROM t WHERE a = $1 AND b = $2 AND a2 = $1");

  let q = parse "-- name: X :one\nSELECT id::text FROM t WHERE id = :id" in
  check ":: cast untouched" (q.Parse.sql = "SELECT id::text FROM t WHERE id = $1");

  let q =
    parse "-- name: X :one\nSELECT ':not_a_param', \":also_not\" FROM t WHERE a = :a"
  in
  check "params in literals untouched"
    (q.Parse.sql = "SELECT ':not_a_param', \":also_not\" FROM t WHERE a = $1");

  let q = parse "-- name: X :one\nSELECT $tag$ :nope $tag$ FROM t WHERE a = :a" in
  check "dollar-quoted body untouched"
    (q.Parse.sql = "SELECT $tag$ :nope $tag$ FROM t WHERE a = $1");

  let q =
    parse
      "-- name: X :one\n\
       SELECT 1 -- :nope\n\
       , 2 /* :nope /* nested */ :nope */ FROM t WHERE a = :a"
  in
  check "comments untouched"
    (q.Parse.sql
   = "SELECT 1 -- :nope\n, 2 /* :nope /* nested */ :nope */ FROM t WHERE a = $1");

  check "mixing $n with :name rejected"
    (fails "-- name: X :one\nSELECT * FROM t WHERE a = $1 AND b = :b");
  let q = parse "-- name: X :one\nSELECT * FROM t WHERE a = $1 AND b = $2" in
  check "pure positional passes through"
    (q.Parse.sql = "SELECT * FROM t WHERE a = $1 AND b = $2" && q.Parse.params = []);

  let q = parse "-- name: X :exec\nUPDATE t SET a = :a?, b = :b" in
  check "nullable marker sets flag"
    (List.map (fun (p : Parse.param) -> (p.Parse.pname, p.Parse.nullable)) q.Parse.params
    = [ ("a", true); ("b", false) ]);
  check "nullable marker stripped from sql" (q.Parse.sql = "UPDATE t SET a = $1, b = $2");

  check "unterminated string rejected" (fails "-- name: X :one\nSELECT 'oops");
  check "unterminated dollar quote rejected" (fails "-- name: X :one\nSELECT $t$ oops");

  (* ---------- optional blocks ---------- *)
  let q =
    parse
      "-- name: X :many\n\
       SELECT a FROM t WHERE o = :o /*? AND e = :e */ /*? AND s = :s */ LIMIT :n"
  in
  let d = Option.get q.Parse.dynamic in
  check "two blocks, four variants"
    (d.Parse.nblocks = 2 && Array.length d.Parse.variant_sqls = 4);
  check "block params attributed" (d.Parse.block_params = [| [ "e" ]; [ "s" ] |]);
  check "full variant is canonical" (names q = [ "o"; "e"; "s"; "n" ]);
  check "variant nparams" (d.Parse.variant_nparams = [| 2; 3; 3; 4 |]);
  check "bare variant renumbers"
    (d.Parse.variant_sqls.(0) = "SELECT a FROM t WHERE o = $1   LIMIT $2");
  check "mask 1 includes first block"
    (d.Parse.variant_sqls.(1) = "SELECT a FROM t WHERE o = $1  AND e = $2   LIMIT $3");
  check "mask 2 includes second block"
    (d.Parse.variant_sqls.(2) = "SELECT a FROM t WHERE o = $1   AND s = $2  LIMIT $3");
  check "full variant"
    (d.Parse.variant_sqls.(3)
   = "SELECT a FROM t WHERE o = $1  AND e = $2   AND s = $3  LIMIT $4");

  check "static query has no dynamic"
    ((parse "-- name: X :one\nSELECT 1").Parse.dynamic = None);
  check "block without params rejected"
    (fails "-- name: X :many\nSELECT a FROM t WHERE o = :o /*? AND b = 1 */");
  check "param shared across blocks rejected"
    (fails "-- name: X :many\nSELECT a FROM t /*? WHERE e = :e */ /*? HAVING e = :e */");
  check "nullable inside block rejected"
    (fails "-- name: X :many\nSELECT a FROM t WHERE o = :o /*? AND e = :e? */");
  check "five blocks rejected"
    (fails
       "-- name: X :many\n\
        SELECT a FROM t WHERE o = :o /*? AND a = :a */ /*? AND b = :b */ /*? AND c = :c \
        */ /*? AND d = :d */ /*? AND e = :e */");
  check "unterminated block rejected" (fails "-- name: X :many\nSELECT a /*? AND b = :b");

  (* ---------- multiple queries per file ---------- *)
  (match
     Parse.of_string ~file:"t.sql"
       "-- name: A :one\nSELECT 1;\n\n-- name: B :exec\nDELETE FROM t"
   with
  | Ok [ a; b ] ->
      check "two queries parse" (a.Parse.name = "A" && b.Parse.name = "B");
      check "trailing semicolon stripped" (a.Parse.sql = "SELECT 1")
  | _ -> check "two queries parse" false);

  print_endline "all good"
