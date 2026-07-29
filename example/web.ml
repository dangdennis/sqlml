(* What a web app's data layer looks like: a connection pool, concurrent
   request handlers, and transactions pinned to one connection.

   The point of this file is what is NOT in it. Compared to app.ml, which used a
   single libpq connection, not one generated function changed. Eio is direct
   style, so a query still returns a plain [result] rather than a promise. *)

open Generated
open Db

let ( let* ) = Result.bind
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

let handle_show pool ~id =
  Sqlml_caqti.Pool.use pool (fun conn -> get_user conn ~id)

let handle_roster pool =
  Sqlml_caqti.Pool.use pool (fun conn ->
      search_users conn ~organization_id:org ~email_pattern:"%@example.com" ~limit:50)

(* ---------- driving it ---------- *)

let people =
  [ (uuid "1b4e28ba-2fa1-11d2-883f-0016d3cca427", "alice@example.com", "Alice")
  ; (uuid "2c5f39cb-3fb2-22e3-994f-1127e4dda538", "bob@example.com", "Bob")
  ; (uuid "3d6a4adc-4fc3-33f4-aa5f-2238f5eee649", "carol@example.com", "Carol")
  ; (uuid "4e7b5bed-5fd4-44f5-bb6f-3349f6fff75a", "dave@example.com", "Dave")
  ]

let unwrap what = function
  | Ok v -> v
  | Error e -> Printf.printf "%s: %s\n" what (Sqlml.Error.to_string e); exit 1

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let uri = Sqlml_caqti.uri_of_env () in
  let pool =
    match Sqlml_caqti.Pool.create ~sw ~stdenv:(env :> Caqti_eio.stdenv) ~max_size:4 uri with
    | Ok p -> p
    | Error e -> prerr_endline (Sqlml.Error.to_string e); exit 1
  in
  Printf.printf "pool     : up (max 4 connections)\n";

  (* clean slate *)
  List.iter
    (fun (id, _, _) ->
      ignore (Sqlml_caqti.Pool.use pool (fun c -> delete_user c ~id)))
    people;

  (* Four signups running concurrently, each on its own pooled connection, each
     in its own transaction. This is the shape of concurrent request handling. *)
  Eio.Fiber.all
    (List.map
       (fun (id, email, display_name) () ->
         match handle_signup pool ~id ~email ~display_name with
         | Ok (Some u) -> Printf.printf "signup   : %s\n" u.email
         | Ok None -> Printf.printf "signup   : %s vanished\n" email
         | Error e -> Printf.printf "signup   : %s failed: %s\n" email (Sqlml.Error.to_string e))
       people);

  (* concurrent reads *)
  Eio.Fiber.all
    (List.map
       (fun (id, email, _) () ->
         match handle_show pool ~id with
         | Ok (Some u) -> Printf.printf "show     : %s -> %s\n" email (Option.value u.display_name ~default:"?")
         | _ -> Printf.printf "show     : %s missing\n" email)
       people);

  let roster = unwrap "roster" (handle_roster pool) in
  Printf.printf "roster   : %d users\n" (List.length roster);

  (* a transaction that fails rolls back, and the connection returns to the pool
     usable -- the classic pooling bug is returning it poisoned *)
  let dup = uuid "5f8c6cfe-6fe5-45f6-cc7f-445af7000a6b" in
  (match handle_signup pool ~id:dup ~email:"alice@example.com" ~display_name:"Impostor" with
   | Ok _ -> print_endline "duplicate: unexpectedly succeeded"
   | Error _ ->
     let still_works = unwrap "after-rollback" (handle_roster pool) in
     Printf.printf "duplicate: rejected; pool still healthy (%d users)\n" (List.length still_works));

  List.iter
    (fun (id, _, _) -> ignore (Sqlml_caqti.Pool.use pool (fun c -> delete_user c ~id)))
    people
