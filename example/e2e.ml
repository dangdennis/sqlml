(* End-to-end: generated code executing against a real Postgres.
   Requires DATABASE_URL (see docker-compose.yml). *)

open Generated

let failures = ref 0

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (Printf.printf "FAIL %s\n" what; incr failures)

let uuid s = Option.get (Uuidm.of_string s)
let id = uuid "1b4e28ba-2fa1-11d2-883f-0016d3cca427"
let org = uuid "6ba7b810-9dad-11d1-80b4-00c04fd430c8"

let () =
  let conn =
    match Sqlml_pg.connect (Sqlml_pg.conninfo_of_env ()) with
    | Ok c -> c
    | Error e -> prerr_endline (Sqlml.Error.to_string e); exit 1
  in

  (* idempotent: this test owns these two ids *)
  ignore (Db.delete_user_exn conn ~id);

  check "insert"
    (Db.create_user_exn conn ~id ~organization_id:org ~email:"e2e@example.com" ~status:Db.Active
       ~balance:(Decimal.of_string "42.50") ~display_name:"End To End" ()
     = 1);

  (* :one, round-tripping every interesting type through the wire *)
  (match Db.get_user_exn conn ~id with
   | None -> check "get_user finds the row" false
   | Some u ->
     check "get_user finds the row" true;
     check "uuid round-trips" (Uuidm.equal u.Db.id id);
     check "text round-trips" (u.Db.email = "e2e@example.com");
     check "nullable set -> Some" (u.Db.display_name = Some "End To End");
     check "enum round-trips" (u.Db.status = Db.Active);
     check "numeric round-trips" (Decimal.to_string u.Db.balance = "42.50");
     check "timestamptz decodes" (Ptime.to_year u.Db.created_at >= 2025));

  (* the shared model type, over the wire *)
  (match Db.get_user_full_exn conn ~id with
   | Some u -> check "shared users_row over the wire" (u.Db.email = "e2e@example.com")
   | None -> check "shared users_row over the wire" false);

  (* :many with ILIKE and a bigint LIMIT *)
  let found = Db.search_users_exn conn ~organization_id:org ~email_pattern:"%@example.com" ~limit:10 in
  check ":many returns the row" (List.exists (fun (r : Db.search_users_row) -> r.Db.email = "e2e@example.com") found);

  (* omitting the optional argument must write a real NULL *)
  check "update" (Db.set_display_name_exn conn ~id () = 1);
  (match Db.get_user_exn conn ~id with
   | Some u -> check "omitted optional -> NULL -> None" (u.Db.display_name = None)
   | None -> check "omitted optional -> NULL -> None" false);

  (* LEFT JOIN: the ? override, against real data. This user has no posts, so
     title is NULL despite posts.title being NOT NULL in the schema -- which is
     exactly what attnotnull alone would have got wrong. *)
  let counts = Db.count_posts_by_user_exn conn in
  (match List.find_opt (fun (r : Db.count_posts_by_user_row) -> r.Db.email = "e2e@example.com") counts with
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
  (match Sqlml.transaction conn (fun tx -> Ok (insert tx ~email:"committed@example.com")) with
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
   | exception Failure _ -> check "raise rolls back and re-raises" (Db.get_user_exn conn ~id = None)
   | _ -> check "raise rolls back and re-raises" false);

  (* generated functions work unchanged inside a transaction *)
  (match
     Sqlml.transaction conn (fun tx ->
         ignore (insert tx ~email:"in-tx@example.com");
         match Db.get_user tx ~id with Ok u -> Ok u | Error e -> Error e)
   with
   | Ok (Some u) -> check "generated query inside tx sees its own write" (u.Db.email = "in-tx@example.com")
   | _ -> check "generated query inside tx sees its own write" false);
  ignore (Db.delete_user_exn conn ~id);

  (* ---------- arrays ---------- *)

  let tag_id = uuid "7a1b2c3d-4e5f-4a6b-8c9d-0e1f2a3b4c5d" in
  (* Elements that exercise every quoting rule Postgres has: a delimiter, a
     quote, braces, a backslash, whitespace, the empty string, and the literal
     text NULL which must not be read back as a null element. *)
  let tricky = [ "plain"; "a,b"; "has \"quote\""; "{braces}"; "back\\slash"; "sp ace"; ""; "NULL" ] in
  check "array insert"
    (Db.put_tag_set_exn conn ~id:tag_id ~owner:org ~tags:tricky ~scores:[ 1; -2; 30 ]
       ~states:[ Db.Active; Db.Banned; Db.Active ]
     = 1);
  (match Db.get_tag_set_exn conn ~id:tag_id with
   | None -> check "array round-trip" false
   | Some t ->
     check "text[] round-trips exactly" (t.Db.tags = tricky);
     check "int[] round-trips" (t.Db.scores = [ 1; -2; 30 ]);
     check "enum[] decodes to variants" (t.Db.states = [ Db.Active; Db.Banned; Db.Active ]));

  (* empty arrays are not the same as NULL *)
  let empty_id = uuid "8b2c3d4e-5f6a-4b7c-9d0e-1f2a3b4c5d6e" in
  ignore (Db.put_tag_set_exn conn ~id:empty_id ~owner:org ~tags:[] ~scores:[] ~states:[]);
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
  check "= ANY with empty array returns nothing" (Db.get_users_by_ids_exn conn ~ids:[] = []);
  List.iter (fun i -> ignore (Db.delete_user_exn conn ~id:i)) [ a; b ];

  if !failures = 0 then print_endline "all good"
  else (Printf.printf "%d failure(s)\n" !failures; exit 1)
