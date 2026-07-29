(* What application code actually looks like against a generated module.

   Nothing here mentions Caqti, libpq, codecs, SQL strings, or modular
   explicits. Run with DATABASE_URL set; see docker-compose.yml. *)

open Generated
open Db

let org = Option.get (Uuidm.of_string "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
let ( let* ) = Result.bind

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
      (Option.value u.display_name ~default:"(no name)")
      u.email
      (match u.status with Active -> "active" | Banned -> "BANNED")
      (Decimal.to_string u.balance)
      (Ptime.to_rfc3339 ~tz_offset_s:0 u.created_at)

let roster conn =
  match search_users conn ~organization_id:org ~email_pattern:"%@example.com" ~limit:50 with
  | Error e -> Printf.sprintf "roster unavailable: %s" (Sqlml.Error.to_string e)
  | Ok [] -> "roster: nobody yet"
  | Ok rows ->
    let one (r : search_users_row) = "  - " ^ r.email in
    "roster:\n" ^ String.concat "\n" (List.map one rows)

(* ---------- driving it ---------- *)

let alice = Option.get (Uuidm.of_string "1b4e28ba-2fa1-11d2-883f-0016d3cca427")
let bob = Option.get (Uuidm.of_string "2c5f39cb-3fb2-22e3-994f-1127e4dda538")
let ghost = Option.get (Uuidm.of_string "00000000-0000-4000-8000-000000000000")

let () =
  let conn =
    match Sqlml_pg.connect (Sqlml_pg.conninfo_of_env ()) with
    | Ok c -> c
    | Error e -> prerr_endline (Sqlml.Error.to_string e); exit 1
  in
  List.iter (fun id -> ignore (delete_user_exn conn ~id)) [ alice; bob ];

  (match signup conn ~id:alice ~email:"alice@example.com" ~display_name:"Alice" with
   | Ok (Some u) -> Printf.printf "signed up : %s\n" u.email
   | Ok None -> print_endline "signed up : vanished?"
   | Error e -> Printf.printf "signup failed: %s\n" (Sqlml.Error.to_string e));

  (match signup conn ~id:bob ~email:"bob@example.com" ~display_name:"Bob" with
   | Ok _ -> print_endline "signed up : bob@example.com"
   | Error e -> Printf.printf "signup failed: %s\n" (Sqlml.Error.to_string e));

  (* a duplicate email violates the unique index -> the whole transaction rolls
     back, and the error arrives as a value rather than an exception *)
  (match signup conn ~id:ghost ~email:"alice@example.com" ~display_name:"Impostor" with
   | Ok _ -> print_endline "duplicate  : unexpectedly succeeded"
   | Error _ ->
     Printf.printf "duplicate  : rejected, and rolled back (ghost exists? %b)\n"
       (get_user_exn conn ~id:ghost <> None));

  print_endline (describe conn ~id:alice);
  print_endline (describe conn ~id:ghost);
  print_endline (roster conn);

  List.iter (fun id -> ignore (delete_user_exn conn ~id)) [ alice; bob ]
