(* What a web app's data layer looks like: a connection pool, concurrent
   request handlers, and transactions pinned to one connection.

   The point of this file is what is NOT in it. Compared to app.ml, which used a
   single libpq connection, not one generated function changed. Eio is direct
   style, so a query still returns a plain [result] rather than a promise. *)

open Generated
open Db

let ( let* ) = Result.bind
let failures = ref 0
let org = Option.get (Uuidm.of_string "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
let uuid s = Option.get (Uuidm.of_string s)

(* ---------- handlers, as a web app would write them ---------- *)

(* Each handler borrows a connection for exactly as long as it needs one. *)
let handle_signup pool ~id ~email ~display_name =
  Sqlml_caqti.Pool.transaction pool @@ fun tx ->
  let* _ =
    create_user tx ~id ~organization_id:org ~email ~status:Active
      ~balance:(Decimal.of_string "0.00") ()
  in
  let* _ = set_display_name tx ~id ~display_name () in
  get_user tx ~id

let handle_show pool ~id = Sqlml_caqti.Pool.use pool (fun conn -> get_user conn ~id)

let handle_roster pool =
  Sqlml_caqti.Pool.use pool (fun conn ->
      search_users conn ~organization_id:org ~email_pattern:"%@example.com" ~limit:50)

(* ---------- driving it ---------- *)

let people =
  [
    (uuid "1b4e28ba-2fa1-11d2-883f-0016d3cca427", "alice@example.com", "Alice");
    (uuid "2c5f39cb-3fb2-22e3-994f-1127e4dda538", "bob@example.com", "Bob");
    (uuid "3d6a4adc-4fc3-33f4-aa5f-2238f5eee649", "carol@example.com", "Carol");
    (uuid "4e7b5bed-5fd4-44f5-bb6f-3349f6fff75a", "dave@example.com", "Dave");
  ]

let unwrap what = function
  | Ok v -> v
  | Error e ->
      Printf.printf "%s: %s\n" what (Sqlml.Error.to_string e);
      incr failures;
      exit 1

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let uri = Sqlml_caqti.uri_of_env () in
  let pool =
    match
      Sqlml_caqti.Pool.create ~sw ~stdenv:(env :> Caqti_eio.stdenv) ~max_size:4 uri
    with
    | Ok p -> p
    | Error e ->
        prerr_endline (Sqlml.Error.to_string e);
        exit 1
  in
  Printf.printf "pool     : up (max 4 connections)\n";

  (* clean slate *)
  List.iter
    (fun (id, _, _) -> ignore (Sqlml_caqti.Pool.use pool (fun c -> delete_user c ~id)))
    people;

  (* Four signups running concurrently, each on its own pooled connection, each
     in its own transaction. This is the shape of concurrent request handling. *)
  Eio.Fiber.all
    (List.map
       (fun (id, email, display_name) () ->
         match handle_signup pool ~id ~email ~display_name with
         | Ok (Some u) -> Printf.printf "signup   : %s\n" u.email
         | Ok None -> Printf.printf "signup   : %s vanished\n" email
         | Error e ->
             incr failures;
             Printf.printf "signup   : %s failed: %s\n" email (Sqlml.Error.to_string e))
       people);

  (* concurrent reads *)
  Eio.Fiber.all
    (List.map
       (fun (id, email, _) () ->
         match handle_show pool ~id with
         | Ok (Some u) ->
             Printf.printf "show     : %s -> %s\n" email
               (Option.value u.name ~default:"?")
         | _ ->
             incr failures;
             Printf.printf "show     : %s missing\n" email)
       people);

  let roster = unwrap "roster" (handle_roster pool) in
  Printf.printf "roster   : %d users\n" (List.length roster);

  (* a transaction that fails rolls back, and the connection returns to the pool
     usable -- the classic pooling bug is returning it poisoned *)
  let dup = uuid "5f8c6cfe-6fe5-45f6-cc7f-445af7000a6b" in
  (match
     handle_signup pool ~id:dup ~email:"alice@example.com" ~display_name:"Impostor"
   with
  | Ok _ -> print_endline "duplicate: unexpectedly succeeded"
  | Error e ->
      (* This is what a handler actually needs: not "it failed", but which
        constraint, so it can return 409 with a useful message instead of 500. *)
      let status =
        match Sqlml.Error.sqlstate e with
        | Some s when Sqlml.Sqlstate.is_unique_violation s -> "409 Conflict"
        | Some s when Sqlml.Sqlstate.is_retryable s -> "retry"
        | Some _ | None -> "500"
      in
      let still_works = unwrap "after-rollback" (handle_roster pool) in
      Printf.printf "duplicate: %s on %s (code %s); pool healthy (%d users)\n" status
        (Option.value (Sqlml.Error.constraint_name e) ~default:"?")
        (match Sqlml.Error.sqlstate e with
        | Some s -> Sqlml.Sqlstate.to_string s
        | None -> "none")
        (List.length still_works));

  (* arrays through the Caqti driver, not just libpq *)
  let tag_id = uuid "9c3d4e5f-6a7b-4c8d-9e0f-1a2b3c4d5e6f" in
  let tags = [ "a,b"; "plain"; "" ] in
  (match
     Sqlml_caqti.Pool.use pool (fun c ->
         let* _ =
           put_tag_set c ~id:tag_id ~owner:org ~tags ~scores:[ 7; 8 ] ~states:[ Active ]
             ~meta:(`Assoc [ ("k", `Int 1) ])
         in
         get_tag_set c ~id:tag_id)
   with
  | Ok (Some t) ->
      if not (t.tags = tags && t.scores = [ 7; 8 ] && t.states = [ Active ]) then
        incr failures;
      Printf.printf "arrays   : tags=%b scores=%b states=%b\n" (t.tags = tags)
        (t.scores = [ 7; 8 ])
        (t.states = [ Active ])
  | Ok None ->
      incr failures;
      print_endline "arrays   : missing"
  | Error e ->
      incr failures;
      Printf.printf "arrays   : %s\n" (Sqlml.Error.to_string e));

  (* streaming through the Caqti driver: DECLARE/FETCH ride the same Driver.S
     operations, so the pool needs nothing special *)
  (match
     Sqlml_caqti.Pool.use pool (fun c ->
         Sqlml.fetch_fold
           (module Search_users)
           ~batch:2 c
           {
             Search_users.organization_id = org;
             email_pattern = "%@example.com";
             limit = 100;
           }
           ~init:0
           ~f:(fun n _ -> n + 1))
   with
  | Ok n -> Printf.printf "stream   : %d row(s) in batches of 2\n" n
  | Error e -> Printf.printf "stream   : %s\n" (Sqlml.Error.to_string e));

  (* jsonb ? operator through the Caqti driver: this used to be re-parsed by
     Caqti's placeholder grammar and misread as a parameter *)
  (match Sqlml_caqti.Pool.use pool (fun c -> has_meta_key c ~key:"k") with
  | Ok rows -> Printf.printf "jsonb ?  : %d row(s) with key\n" (List.length rows)
  | Error e ->
      incr failures;
      Printf.printf "jsonb ?  : %s\n" (Sqlml.Error.to_string e));

  List.iter
    (fun (id, _, _) -> ignore (Sqlml_caqti.Pool.use pool (fun c -> delete_user c ~id)))
    people;
  if !failures > 0 then exit 1
