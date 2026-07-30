(* End-to-end: generated code executing against a real Postgres.
   Requires DATABASE_URL (see docker-compose.yml). *)

open Generated

let failures = ref 0

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    incr failures)

let uuid s = Option.get (Uuidm.of_string s)

(* jsonb is a normalised representation: Postgres reorders object keys by length
   then bytes, re-spaces, and drops duplicate keys. So a round-trip preserves
   the value, not the text. Compare structurally. Use `json` rather than `jsonb`
   if byte-exact preservation matters. *)
let rec json_sorted : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Assoc kvs ->
      `Assoc (List.sort compare (List.map (fun (k, v) -> (k, json_sorted v)) kvs))
  | `List l -> `List (List.map json_sorted l)
  | v -> v

let id = uuid "1b4e28ba-2fa1-11d2-883f-0016d3cca427"
let org = uuid "6ba7b810-9dad-11d1-80b4-00c04fd430c8"

let () =
  let conn =
    match Sqlml_pg.connect (Sqlml_pg.conninfo_of_env ()) with
    | Ok c -> c
    | Error e ->
        prerr_endline (Sqlml.Error.to_string e);
        exit 1
  in

  (* idempotent: this test owns these two ids *)
  ignore (Db.delete_user_exn conn ~id);

  check "insert"
    (Db.create_user_exn conn ~id ~organization_id:org ~email:"e2e@example.com"
       ~status:Db.Active ~balance:(Decimal.of_string "42.50") ~display_name:"End To End"
       ()
    = 1);

  (* :one, round-tripping every interesting type through the wire *)
  (match Db.get_user_exn conn ~id with
  | None -> check "get_user finds the row" false
  | Some u ->
      check "get_user finds the row" true;
      check "uuid round-trips" (Uuidm.equal u.Db.id id);
      check "text round-trips" (u.Db.email = "e2e@example.com");
      check "nullable set -> Some" (u.Db.name = Some "End To End");
      check "enum round-trips" (u.Db.status = Db.Active);
      check "numeric round-trips" (Decimal.to_string u.Db.balance = "42.50");
      check "timestamptz decodes" (Ptime.to_year u.Db.created_at >= 2025));

  (* the shared model type, over the wire *)
  (match Db.get_user_full_exn conn ~id with
  | Some u -> check "shared users_row over the wire" (u.Db.email = "e2e@example.com")
  | None -> check "shared users_row over the wire" false);

  (* :many with ILIKE and a bigint LIMIT *)
  let found =
    Db.search_users_exn conn ~organization_id:org ~email_pattern:"%@example.com" ~limit:10
  in
  check ":many returns the row"
    (List.exists (fun (r : Db.search_users_row) -> r.Db.email = "e2e@example.com") found);

  (* omitting the optional argument must write a real NULL *)
  check "update" (Db.set_display_name_exn conn ~id () = 1);
  (match Db.get_user_exn conn ~id with
  | Some u -> check "omitted optional -> NULL -> None" (u.Db.name = None)
  | None -> check "omitted optional -> NULL -> None" false);

  (* LEFT JOIN: the ? override, against real data. This user has no posts, so
     title is NULL despite posts.title being NOT NULL in the schema -- which is
     exactly what attnotnull alone would have got wrong. *)
  let counts = Db.count_posts_by_user_exn conn in
  (match
     List.find_opt
       (fun (r : Db.count_posts_by_user_row) -> r.Db.email = "e2e@example.com")
       counts
   with
  | Some r ->
      check "LEFT JOIN title is None" (r.Db.title = None);
      check "! override gives a plain int" (r.Db.post_count = 0)
  | None -> check "LEFT JOIN row present" false);

  check "delete" (Db.delete_user_exn conn ~id = 1);
  check "deleted row is gone" (Db.get_user_exn conn ~id = None);

  (* ---------- transactions ---------- *)
  let insert c ~email =
    Db.create_user_exn c ~id ~organization_id:org ~email ~status:Db.Active
      ~balance:(Decimal.of_string "1.00") ()
  in

  (* commits when the body returns Ok *)
  (match
     Sqlml.transaction conn (fun tx -> Ok (insert tx ~email:"committed@example.com"))
   with
  | Ok 1 -> check "transaction commits" (Db.get_user_exn conn ~id <> None)
  | _ -> check "transaction commits" false);
  ignore (Db.delete_user_exn conn ~id);

  (* rolls back when the body returns Error *)
  (match
     Sqlml.transaction conn (fun tx ->
         ignore (insert tx ~email:"rolled-back@example.com");
         Error (Sqlml.Error.Connect "deliberate"))
   with
  | Error _ -> check "Error rolls back" (Db.get_user_exn conn ~id = None)
  | Ok _ -> check "Error rolls back" false);

  (* rolls back when the body raises, and re-raises *)
  (match
     Sqlml.transaction conn (fun tx ->
         ignore (insert tx ~email:"raised@example.com");
         failwith "boom")
   with
  | exception Failure _ ->
      check "raise rolls back and re-raises" (Db.get_user_exn conn ~id = None)
  | _ -> check "raise rolls back and re-raises" false);

  (* generated functions work unchanged inside a transaction *)
  (match
     Sqlml.transaction conn (fun tx ->
         ignore (insert tx ~email:"in-tx@example.com");
         match Db.get_user tx ~id with Ok u -> Ok u | Error e -> Error e)
   with
  | Ok (Some u) ->
      check "generated query inside tx sees its own write"
        (u.Db.email = "in-tx@example.com")
  | _ -> check "generated query inside tx sees its own write" false);
  ignore (Db.delete_user_exn conn ~id);

  (* ---------- arrays ---------- *)
  let tag_id = uuid "7a1b2c3d-4e5f-4a6b-8c9d-0e1f2a3b4c5d" in
  (* Elements that exercise every quoting rule Postgres has: a delimiter, a
     quote, braces, a backslash, whitespace, the empty string, and the literal
     text NULL which must not be read back as a null element. *)
  let tricky =
    [ "plain"; "a,b"; "has \"quote\""; "{braces}"; "back\\slash"; "sp ace"; ""; "NULL" ]
  in
  let meta = Yojson.Safe.from_string {|{"nested":{"a":[1,2,null]},"s":"x"}|} in
  check "array insert"
    (Db.put_tag_set_exn conn ~id:tag_id ~owner:org ~tags:tricky ~scores:[ 1; -2; 30 ]
       ~states:[ Db.Active; Db.Banned; Db.Active ]
       ~meta
    = 1);
  (match Db.get_tag_set_exn conn ~id:tag_id with
  | None -> check "array round-trip" false
  | Some t ->
      check "text[] round-trips exactly" (t.Db.tags = tricky);
      check "int[] round-trips" (t.Db.scores = [ 1; -2; 30 ]);
      check "enum[] decodes to variants"
        (t.Db.states = [ Db.Active; Db.Banned; Db.Active ]);
      check "jsonb round-trips (structurally)" (json_sorted t.Db.meta = json_sorted meta));

  (* empty arrays are not the same as NULL *)
  let empty_id = uuid "8b2c3d4e-5f6a-4b7c-9d0e-1f2a3b4c5d6e" in
  ignore
    (Db.put_tag_set_exn conn ~id:empty_id ~owner:org ~tags:[] ~scores:[] ~states:[]
       ~meta:(`Assoc []));
  (match Db.get_tag_set_exn conn ~id:empty_id with
  | Some t -> check "empty array round-trips" (t.Db.tags = [] && t.Db.scores = [])
  | None -> check "empty array round-trips" false);

  (* an array parameter: = ANY(...) as a dynamic IN list *)
  let a = uuid "11111111-1111-4111-8111-111111111111" in
  let b = uuid "22222222-2222-4222-8222-222222222222" in
  List.iter
    (fun (i, e) ->
      ignore
        (Db.create_user_exn conn ~id:i ~organization_id:org ~email:e ~status:Db.Active
           ~balance:(Decimal.of_string "0.00") ()))
    [ (a, "any-a@example.com"); (b, "any-b@example.com") ];
  let found = Db.get_users_by_ids_exn conn ~ids:[ a; b ] in
  check "= ANY(array param) matches both" (List.length found = 2);
  check "= ANY with empty array returns nothing"
    (Db.get_users_by_ids_exn conn ~ids:[] = []);
  List.iter (fun i -> ignore (Db.delete_user_exn conn ~id:i)) [ a; b ];

  (* ---------- SQLSTATE ---------- *)
  let sqlstate_of r = match r with Error e -> Sqlml.Error.sqlstate e | Ok _ -> None in
  let a2 = uuid "aaaaaaaa-0000-4000-8000-000000000001" in
  ignore
    (Db.create_user_exn conn ~id:a2 ~organization_id:org ~email:"dup@example.com"
       ~status:Db.Active ~balance:(Decimal.of_string "0.00") ());

  (* unique violation on email *)
  let dup =
    Db.create_user conn
      ~id:(uuid "aaaaaaaa-0000-4000-8000-000000000002")
      ~organization_id:org ~email:"dup@example.com" ~status:Db.Active
      ~balance:(Decimal.of_string "0.00") ()
  in
  (match sqlstate_of dup with
  | Some s ->
      check "unique violation is 23505" (Sqlml.Sqlstate.to_string s = "23505");
      check "named unique_violation" (Sqlml.Sqlstate.name s = "unique_violation");
      check "condition matches"
        (Sqlml.Sqlstate.condition s = Sqlml.Sqlstate.Unique_violation);
      check "classified as integrity" (Sqlml.Sqlstate.is_integrity_violation s);
      check "not retryable" (not (Sqlml.Sqlstate.is_retryable s));
      check "class is 23"
        (Sqlml.Sqlstate.class_ s = Sqlml.Sqlstate.Class.Integrity_constraint_violation)
  | None -> check "unique violation reports a sqlstate" false);
  (match dup with
  | Error e ->
      check "constraint name is reported"
        (Sqlml.Error.constraint_name e = Some "users_email_key");
      check "Error.is_unique_violation" (Sqlml.Error.is_unique_violation e)
  | Ok _ -> check "duplicate insert failed" false);

  (* foreign key violation: a post whose author does not exist *)
  let fk =
    Db.put_tag_set conn
      ~id:(uuid "bbbbbbbb-0000-4000-8000-000000000001")
      ~owner:org ~tags:[] ~scores:[] ~states:[] ~meta:(`Assoc [])
  in
  ignore fk;
  ignore (Db.delete_user_exn conn ~id:a2);

  (* ---------- nesting: savepoints ---------- *)
  let n1 = uuid "cccccccc-0000-4000-8000-000000000001" in
  let n2 = uuid "cccccccc-0000-4000-8000-000000000002" in
  let n3 = uuid "cccccccc-0000-4000-8000-000000000003" in
  let mk tx i email =
    Db.create_user tx ~id:i ~organization_id:org ~email ~status:Db.Active
      ~balance:(Decimal.of_string "0.00") ()
  in
  List.iter (fun i -> ignore (Db.delete_user_exn conn ~id:i)) [ n1; n2; n3 ];
  (match
     Sqlml.transaction conn (fun tx ->
         match mk tx n1 "outer-a@example.com" with
         | Error e -> Error e
         | Ok _ ->
             (* inner failure must roll back only the inner insert *)
             (match
                Sqlml.transaction tx (fun inner ->
                    match mk inner n2 "inner@example.com" with
                    | Error e -> Error e
                    | Ok _ -> Error (Sqlml.Error.Connect "abort inner"))
              with
             | Ok _ -> ()
             | Error _ -> ());
             mk tx n3 "outer-b@example.com")
   with
  | Ok _ ->
      check "outer survives inner rollback" (Db.get_user_exn conn ~id:n1 <> None);
      check "inner work rolled back" (Db.get_user_exn conn ~id:n2 = None);
      check "outer continues after inner rollback" (Db.get_user_exn conn ~id:n3 <> None)
  | Error e ->
      print_endline (Sqlml.Error.to_string e);
      check "nested transaction" false);
  List.iter (fun i -> ignore (Db.delete_user_exn conn ~id:i)) [ n1; n3 ];

  (* isolation inside an enclosing transaction is a programming error *)
  (match
     Sqlml.transaction conn (fun tx ->
         Sqlml.transaction ~isolation:`Serializable tx (fun _ -> Ok ()))
   with
  | exception Invalid_argument _ -> check "nested ~isolation raises Invalid_argument" true
  | _ -> check "nested ~isolation raises Invalid_argument" false);

  (* ---------- retry: a real serialization failure ---------- *)

  (* Two connections race on one row under REPEATABLE READ: conn2 updates the
     row after conn1 has taken its snapshot but before conn1 writes, which is a
     deterministic 40001 "could not serialize access due to concurrent update".
     The retry re-runs the body on a fresh snapshot, which succeeds. *)
  let victim = uuid "dddddddd-0000-4000-8000-000000000001" in
  ignore (Db.delete_user_exn conn ~id:victim);
  ignore
    (Db.create_user_exn conn ~id:victim ~organization_id:org ~email:"racer@example.com"
       ~status:Db.Active ~balance:(Decimal.of_string "0.00") ());
  let conn2 =
    match Sqlml_pg.connect (Sqlml_pg.conninfo_of_env ()) with
    | Ok c -> c
    | Error e ->
        print_endline (Sqlml.Error.to_string e);
        exit 1
  in
  let attempts = ref 0 in
  (match
     Sqlml.transaction ~isolation:`Repeatable_read ~retry:2 conn (fun tx ->
         incr attempts;
         (* take the snapshot *)
         match Db.get_user tx ~id:victim with
         | Error e -> Error e
         | Ok None -> Error (Sqlml.Error.Connect "victim vanished")
         | Ok (Some _) ->
             (* on the first attempt only, interfere from the second connection *)
             if !attempts = 1 then
               ignore
                 (Db.set_display_name_exn conn2 ~id:victim ~display_name:"interfered" ());
             Db.set_display_name tx ~id:victim
               ~display_name:(Printf.sprintf "attempt-%d" !attempts)
               ())
   with
  | Ok _ ->
      check "serialization failure retried" (!attempts = 2);
      check "retry succeeded on fresh snapshot"
        (match Db.get_user_exn conn ~id:victim with
        | Some u -> u.Db.name = Some "attempt-2"
        | None -> false)
  | Error e ->
      print_endline (Sqlml.Error.to_string e);
      check "serialization failure retried" false);
  ignore (Db.delete_user_exn conn ~id:victim);

  (* ---------- :one! ---------- *)
  let s1 = uuid "eeeeeeee-0000-4000-8000-000000000001" in
  ignore (Db.delete_user_exn conn ~id:s1);
  ignore
    (Db.create_user_exn conn ~id:s1 ~organization_id:org ~email:"strict@example.com"
       ~status:Db.Active ~balance:(Decimal.of_string "0.00") ());
  (match Db.get_user_strict conn ~id:s1 with
  | Ok u -> check ":one! returns the row unwrapped" (u.Db.email = "strict@example.com")
  | Error _ -> check ":one! returns the row unwrapped" false);
  ignore (Db.delete_user_exn conn ~id:s1);
  (match Db.get_user_strict conn ~id:s1 with
  | Error (Sqlml.Error.Cardinality { expected = "exactly 1"; got = 0; _ }) ->
      check ":one! absence is a Cardinality error" true
  | _ -> check ":one! absence is a Cardinality error" false);

  (* ---------- streaming ---------- *)
  let ids =
    List.map
      (fun i -> uuid (Printf.sprintf "ffffffff-0000-4000-8000-%012d" i))
      [ 1; 2; 3; 4; 5 ]
  in
  List.iteri
    (fun i id ->
      ignore (Db.delete_user_exn conn ~id);
      ignore
        (Db.create_user_exn conn ~id ~organization_id:org
           ~email:(Printf.sprintf "stream-%d@example.com" i)
           ~status:Db.Active ~balance:(Decimal.of_string "0.00") ()))
    ids;

  (* batch 2 over 5 rows forces three FETCHes, so batching is actually exercised *)
  (match
     Sqlml.fetch_fold
       (module Db.Search_users)
       ~batch:2 conn
       { Db.Search_users.organization_id = org; email_pattern = "stream-%"; limit = 100 }
       ~init:0
       ~f:(fun n _ -> n + 1)
   with
  | Ok n -> check "fetch_fold sees all rows across batches" (n = 5)
  | Error e ->
      print_endline (Sqlml.Error.to_string e);
      check "fetch_fold sees all rows across batches" false);

  (* streaming inside an enclosing transaction: the cursor nests via savepoint *)
  (match
     Sqlml.transaction conn (fun tx ->
         Sqlml.fetch_fold
           (module Db.Search_users)
           ~batch:2 tx
           {
             Db.Search_users.organization_id = org;
             email_pattern = "stream-%";
             limit = 100;
           }
           ~init:[]
           ~f:(fun acc r -> r.Db.email :: acc))
   with
  | Ok emails -> check "streaming inside a transaction" (List.length emails = 5)
  | Error e ->
      print_endline (Sqlml.Error.to_string e);
      check "streaming inside a transaction" false);

  List.iter (fun id -> ignore (Db.delete_user_exn conn ~id)) ids;

  (* ---------- statement cache vs rollback ----------

     The libpq driver prepares each SQL text on first use. If that first use
     happens inside a transaction that rolls back, the server deallocates the
     prepared statement while the cache still remembers it -- the next call gets
     26000 invalid_sql_statement_name unless the driver re-prepares. Run a
     never-before-used query inside a rolled-back transaction, then again
     outside. *)
  ignore
    (Sqlml.transaction conn (fun tx ->
         match Db.count_users_by_status tx with
         | Ok _ -> Error (Sqlml.Error.Connect "deliberate rollback")
         | Error e -> Error e));
  (match Db.count_users_by_status conn with
  | Ok _ -> check "prepared stmt survives rollback of its first use" true
  | Error e ->
      print_endline (Sqlml.Error.to_string e);
      check "prepared stmt survives rollback of its first use" false);

  (* and the plain repeated-use path: same statement, three executions *)
  let ok3 =
    List.for_all
      (fun _ ->
        match Db.count_users_by_status conn with Ok _ -> true | Error _ -> false)
      [ 1; 2; 3 ]
  in
  check "repeated executions hit the cache" ok3;

  (* ---------- optional blocks ---------- *)
  let mk i email st bal =
    ignore (Db.delete_user_exn conn ~id:i);
    ignore
      (Db.create_user_exn conn ~id:i ~organization_id:org ~email ~status:st
         ~balance:(Decimal.of_string bal) ())
  in
  let d1 = uuid "abababab-0000-4000-8000-000000000001" in
  let d2 = uuid "abababab-0000-4000-8000-000000000002" in
  let d3 = uuid "abababab-0000-4000-8000-000000000003" in
  mk d1 "dyn-a@example.com" Db.Active "5.00";
  mk d2 "dyn-b@example.com" Db.Active "50.00";
  mk d3 "dyn-c@other.org" Db.Banned "500.00";

  let count ?email ?status ?min_balance () =
    match Db.find_users conn ~org ~limit:100 ?email ?status ?min_balance () with
    | Ok rows ->
        List.length
          (List.filter
             (fun (r : Db.find_users_row) ->
               String.length r.Db.email >= 4 && String.sub r.Db.email 0 4 = "dyn-")
             rows)
    | Error e ->
        print_endline (Sqlml.Error.to_string e);
        -1
  in
  check "no filters -> all 3" (count () = 3);
  check "email filter" (count ~email:"%@example.com" () = 2);
  check "status filter (enum param in a block)" (count ~status:Db.Banned () = 1);
  check "numeric filter" (count ~min_balance:(Decimal.of_string "40") () = 2);
  check "two filters combine"
    (count ~email:"%@example.com" ~min_balance:(Decimal.of_string "40") () = 1);
  check "all three filters"
    (count ~email:"%@%" ~status:Db.Active ~min_balance:(Decimal.of_string "1") () = 2);
  List.iter (fun i -> ignore (Db.delete_user_exn conn ~id:i)) [ d1; d2; d3 ];

  if !failures = 0 then print_endline "all good"
  else (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1)
