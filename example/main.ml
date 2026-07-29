(* Realistic call sites for the agreed generated API, against a stub driver so
   the example runs with no database. Replace [Stub] with the Caqti driver and
   nothing above it changes. *)

module Stub = struct
  type conn = unit

  let name = "stub"
  let placeholder n = "$" ^ string_of_int n

  let contains hay needle =
    let nh = String.length hay and nn = String.length needle in
    let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
    go 0

  let user =
    [| Sqlml.Value.Text "1b4e28ba-2fa1-11d2-883f-0016d3cca427"
     ; Sqlml.Value.Text "dennis@example.com"
     ; Sqlml.Value.Null
     ; Sqlml.Value.Text "active"
     ; Sqlml.Value.Text "1234.50"
     ; (* exactly how Postgres prints timestamptz: space, and a 2-digit offset *)
       Sqlml.Value.Text "2026-07-28 09:00:00+00"
    |]

  let ts = Sqlml.Value.Text "2026-07-28 09:00:00+00"
  let uu = Sqlml.Value.Text "1b4e28ba-2fa1-11d2-883f-0016d3cca427"

  (* the 25-column users row *)
  let wide =
    [| uu; uu; Sqlml.Value.Text "dennis@example.com"; Sqlml.Value.Null
     ; Sqlml.Value.Text "Dennis"; Sqlml.Value.Null; Sqlml.Value.Null; Sqlml.Value.Null
     ; Sqlml.Value.Text "en"; Sqlml.Value.Text "UTC"
     ; Sqlml.Value.Text "active"; Sqlml.Value.Text "owner"
     ; Sqlml.Value.Text "1234.50"; Sqlml.Value.Null
     ; Sqlml.Value.Int 42; Sqlml.Value.Int 0
     ; ts; Sqlml.Value.Null; Sqlml.Value.Null
     ; Sqlml.Value.Bool false; Sqlml.Value.Bool true
     ; Sqlml.Value.Text "{}"; ts; ts; Sqlml.Value.Null
    |]

  let query () ~sql ~params:_ ~columns:_ =
    if contains sql "email_verified_at" then Ok [ wide ]
    else if contains sql "display_name, status\n" then
      Ok [ [| uu; Sqlml.Value.Text "dennis@example.com"; Sqlml.Value.Text "Dennis"; Sqlml.Value.Text "active" |] ]
    else if contains sql "display_name" then Ok [ user ]
    else if contains sql "post_count" then
      Ok
        [ [| Sqlml.Value.Text "dennis@example.com"; Sqlml.Value.Null; Sqlml.Value.Int 0 |]
        ; [| Sqlml.Value.Text "ann@example.com"; Sqlml.Value.Text "Hello"; Sqlml.Value.Int 3 |]
        ]
    else
      Ok
        [ [| Sqlml.Value.Text "1b4e28ba-2fa1-11d2-883f-0016d3cca427"
           ; Sqlml.Value.Text "dennis@example.com"
           ; Sqlml.Value.Text "2026-07-28 09:00:00+00"
          |]
        ]

  let exec () ~sql:_ ~params:_ = Ok 1
end

let conn = Sqlml.Driver.make (module Stub) ()
let uuid s = Option.get (Uuidm.of_string s)

(* One open at the top of the file. Every row's fields are now in scope. *)
open Db

let () =
  let id = uuid "1b4e28ba-2fa1-11d2-883f-0016d3cca427" in

  (* Raising style -- the common path reads like ordinary code. *)
  (match get_user_exn conn ~id with
   | Some u ->
     Printf.printf "user   : %s  balance=%s  at=%s  display_name=%s\n" u.email
       (Decimal.to_string u.balance)
       (Ptime.to_rfc3339 u.created_at)
       (Option.value u.display_name ~default:"NULL")
   | None -> print_endline "user   : not found");

  (* Default variant returns a result, for when you want to handle failure. *)
  (match search_users conn ~organization_id:id ~email_pattern:"%@example.com" ~limit:10 with
   | Ok rows -> Printf.printf "search : %d row(s), first = %s\n" (List.length rows) (List.hd rows).email
   | Error e -> Printf.printf "search : failed: %s\n" (Sqlml.Error.to_string e));

  (* Nullability from the LEFT JOIN, and the "!"-pinned computed column. *)
  List.iter
    (fun r ->
      Printf.printf "posts  : %-20s title=%-8s count=%d\n" r.email
        (Option.value r.title ~default:"NULL")
        r.post_count)
    (count_posts_by_user_exn conn);

  Printf.printf "deleted: %d\n" (delete_user_exn conn ~id);

  (* The query modules are still exported, so generic code works. Nothing in an
     application needs this -- it is for tooling, tracing, batch runners. *)
  (match Sqlml.fetch_one {Db.Get_user} conn { Db.Get_user.id } with
   | Ok (Some u) -> Printf.printf "generic: %s\n" u.email
   | Ok None -> print_endline "generic: none"
   | Error e -> Printf.printf "generic: failed: %s\n" (Sqlml.Error.to_string e));

  (* And the raising variant really does raise. *)
  (match Sqlml.fetch_one {Db.Get_user} conn { Db.Get_user.id } with
   | Error e -> raise (Sqlml.Sql_error e)
   | Ok _ -> print_endline "done");

  (* ---------- the same API over a 25-column table ---------- *)
  print_endline "";
  print_endline "== wide table (25 columns) ==";

  (* One helper, written once against the shared model type, works for every
     query that selects the full users row. Before the shared type this would
     have needed a copy per query. *)
  let describe (u : Wide.users_row) =
    Printf.sprintf "%s <%s>" (Option.value u.Wide.display_name ~default:"?") u.Wide.email
  in
  (match Wide.get_user_exn conn ~id with
   | Some u -> print_endline ("shared : by id    -> " ^ describe u)
   | None -> ());
  (match Wide.get_user_by_email_exn conn ~email:"dennis@example.com" with
   | Some u -> print_endline ("shared : by email -> " ^ describe u)
   | None -> ());

  (* Reading is unaffected by width: you name the fields you want. *)
  (match Wide.get_user_exn conn ~id with
   | Some u ->
     Printf.printf "read   : %s  role=%s  logins=%d  balance=%s\n" u.Wide.email
       (match u.Wide.role with Wide.Owner -> "owner" | Admin -> "admin" | Member -> "member" | Guest -> "guest")
       u.Wide.login_count
       (Decimal.to_string u.Wide.balance)
   | None -> print_endline "read   : not found");

  (* A narrow projection gets its own small row type. *)
  (match Wide.list_user_summaries conn ~organization_id:id ~limit:50 with
   | Ok rows -> Printf.printf "summary: %d row(s), first = %s\n" (List.length rows) (List.hd rows).Wide.email
   | Error e -> Printf.printf "summary: %s\n" (Sqlml.Error.to_string e));

  (* Nullable params are optional, so the four Nones are simply omitted. Any
     query with a nullable parameter ends in (). *)
  let n =
    Wide.create_user_exn conn ~organization_id:id ~email:"new@example.com" ~locale:"en"
      ~timezone:"UTC" ~status:Wide.Active ~role:Wide.Member ~marketing_opt_in:false
      ~metadata:"{}" ~display_name:"New Person" ()
  in
  Printf.printf "insert : %d row(s)\n" n
