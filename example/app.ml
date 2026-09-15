(* What application code actually looks like against a generated module.

   Nothing here mentions Caqti, libpq, codecs, SQL strings, or modular
   explicits. Run with DATABASE_URL set; see docker-compose.yml. *)

open Generated
open Db

(* App-owned constants; e2e.ml and web.ml use different orgs, ids and email
   domains so the programs stay independent. *)
let org = Option.get (Uuidm.of_string "0a000000-0000-4000-8000-000000000001")
let ( let* ) = Result.bind
let failures = ref 0

let fail_with e =
  incr failures;
  Sqlml.Error.to_string e

(* ---------- a service layer ---------- *)

(* Signing up is two statements that must land together, so it goes in a
   transaction. Generated functions take the transaction handle unchanged. *)
let signup conn ~id ~email ~display_name =
  Sqlml.transaction conn @@ fun tx ->
  let* _ =
    create_user tx ~id ~organization_id:org ~email ~status:Active
      ~balance:(Decimal.of_string "0.00") ()
  in
  let* _ = set_display_name tx ~id ~display_name () in
  get_user tx ~id

(* A caller that genuinely cannot proceed on failure uses the _exn variant and
   lets Sqlml.Sql_error propagate. *)
let describe conn ~id =
  match get_user_exn conn ~id with
  | None -> Printf.sprintf "user %s: not found" (Uuidm.to_string id)
  | Some u ->
      Printf.sprintf "%s <%s> %s balance=%s joined=%s"
        (Option.value u.name ~default:"(no name)")
        (Option.get u.email)
        (match Option.get u.status with Active -> "active" | Banned -> "BANNED")
        (Decimal.to_string (Option.get u.balance))
        (Ptime.to_rfc3339 ~tz_offset_s:0 (Option.get u.created_at))

let roster conn =
  match
    search_users conn ~organization_id:org ~email_pattern:"%@app.example.com" ~limit:50L
  with
  | Error e -> Printf.sprintf "roster unavailable: %s" (fail_with e)
  | Ok [] -> "roster: nobody yet"
  | Ok rows ->
      let one (r : search_users_row) = "  - " ^ Option.get r.email in
      "roster:\n" ^ String.concat "\n" (List.map one rows)

(* ---------- driving it ---------- *)

let alice = Option.get (Uuidm.of_string "0a000000-0000-4000-8000-000000000011")
let bob = Option.get (Uuidm.of_string "0a000000-0000-4000-8000-000000000012")
let ghost = Option.get (Uuidm.of_string "00000000-0000-4000-8000-000000000000")

let () =
  let conn =
    match Sqlml_pg.connect (Sqlml_pg.conninfo_of_env ()) with
    | Ok c -> c
    | Error e ->
        prerr_endline (Sqlml.Error.to_string e);
        exit 1
  in
  List.iter (fun id -> ignore (delete_user_exn conn ~id)) [ alice; bob ];

  (match signup conn ~id:alice ~email:"alice@app.example.com" ~display_name:"Alice" with
  | Ok (Some u) -> Printf.printf "signed up : %s\n" (Option.get u.email)
  | Ok None -> print_endline "signed up : vanished?"
  | Error e -> Printf.printf "signup failed: %s\n" (fail_with e));

  (match signup conn ~id:bob ~email:"bob@app.example.com" ~display_name:"Bob" with
  | Ok _ -> print_endline "signed up : bob@app.example.com"
  | Error e -> Printf.printf "signup failed: %s\n" (fail_with e));

  (* a duplicate email violates the unique index -> the whole transaction rolls
     back, and the error arrives as a value rather than an exception *)
  (match
     signup conn ~id:ghost ~email:"alice@app.example.com" ~display_name:"Impostor"
   with
  | Ok _ -> print_endline "duplicate  : unexpectedly succeeded"
  | Error _ ->
      Printf.printf "duplicate  : rejected, and rolled back (ghost exists? %b)\n"
        (get_user_exn conn ~id:ghost <> None));

  print_endline (describe conn ~id:alice);
  print_endline (describe conn ~id:ghost);
  print_endline (roster conn);

  List.iter (fun id -> ignore (delete_user_exn conn ~id)) [ alice; bob ];
  if !failures > 0 then exit 1
