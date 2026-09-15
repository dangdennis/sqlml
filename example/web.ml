(* What a web app's data layer looks like: a connection pool, concurrent
   request handlers, and transactions pinned to one connection.

   The point of this file is what is NOT in it. Compared to app.ml, which used a
   single libpq connection, not one generated function changed. Eio is direct
   style, so a query still returns a plain [result] rather than a promise. *)

open Generated
open Db

let ( let* ) = Result.bind
let failures = ref 0

(* Constants are web-owned: e2e.ml and app.ml use different orgs, ids and
   email domains, so the three programs cannot corrupt each other's data or
   rosters no matter which order they run in. *)
let org = Option.get (Uuidm.of_string "0b000000-0000-4000-8000-000000000001")
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
      search_users conn ~organization_id:org ~email_pattern:"%@web.example.com" ~limit:50L)

(* ---------- driving it ---------- *)

let people =
  [
    (uuid "0b000000-0000-4000-8000-000000000011", "alice@web.example.com", "Alice");
    (uuid "0b000000-0000-4000-8000-000000000012", "bob@web.example.com", "Bob");
    (uuid "0b000000-0000-4000-8000-000000000013", "carol@web.example.com", "Carol");
    (uuid "0b000000-0000-4000-8000-000000000014", "dave@web.example.com", "Dave");
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
         | Ok (Some u) -> Printf.printf "signup   : %s\n" (Option.get u.email)
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
     handle_signup pool ~id:dup ~email:"alice@web.example.com" ~display_name:"Impostor"
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
           put_tag_set c ~id:tag_id ~owner:org ~tags:(Sqlml.Pg_array.of_list tags)
             ~scores:(Sqlml.Pg_array.of_list [ 7; 8 ])
             ~states:(Sqlml.Pg_array.of_list [ Active ])
             ~meta:(`Assoc [ ("k", `Int 1) ])
         in
         get_tag_set c ~id:tag_id)
   with
  | Ok (Some t) ->
      if
        not
          (Sqlml.Pg_array.to_list (Option.get t.tags) = tags
          && Sqlml.Pg_array.to_list (Option.get t.scores) = [ 7; 8 ]
          && Sqlml.Pg_array.to_list (Option.get t.states) = [ Active ])
      then incr failures;
      Printf.printf "arrays   : tags=%b scores=%b states=%b\n"
        (Sqlml.Pg_array.to_list (Option.get t.tags) = tags)
        (Sqlml.Pg_array.to_list (Option.get t.scores) = [ 7; 8 ])
        (Sqlml.Pg_array.to_list (Option.get t.states) = [ Active ])
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
             email_pattern = "%@web.example.com";
             limit = 100L;
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

  (* COPY is libpq-only by design; the Caqti driver must say so clearly
     rather than fail obscurely *)
  (match
     Sqlml_caqti.Pool.use pool (fun c ->
         bulk_add_users c
           [
             {
               Bulk_add_users.id = uuid "0b000000-0000-4000-8000-0000000000c1";
               organization_id = org;
               email = "copy@web.example.com";
               display_name = None;
               status = Active;
               balance = Decimal.of_string "0.00";
             };
           ])
   with
  | Error e ->
      let m = Sqlml.Error.to_string e in
      let says_libpq =
        let rec go i =
          i + 5 <= String.length m && (String.sub m i 5 = "libpq" || go (i + 1))
        in
        go 0
      in
      if not says_libpq then incr failures;
      Printf.printf "copy     : unsupported here, error names the fix (%b)\n" says_libpq
  | Ok _ ->
      incr failures;
      print_endline "copy     : unexpectedly succeeded on Caqti");

  List.iter
    (fun (id, _, _) -> ignore (Sqlml_caqti.Pool.use pool (fun c -> delete_user c ~id)))
    people;
  ignore
    (unwrap "compiler codecs"
       (Sqlml_caqti.Pool.use pool (fun conn ->
            Compiler_checks.run ~id:902
              ~check:(fun what ok ->
                Printf.printf "%s: %b\n" what ok;
                if not ok then incr failures)
              conn;
            Ok ())));
  if !failures > 0 then exit 1
