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

  let query () ~sql ~params:_ =
    if contains sql "display_name" then Ok [ user ]
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
  (match get_user conn ~id with
   | Some u ->
     Printf.printf "user   : %s  balance=%s  at=%s  display_name=%s\n" u.email
       (Decimal.to_string u.balance)
       (Ptime.to_rfc3339 u.created_at)
       (Option.value u.display_name ~default:"NULL")
   | None -> print_endline "user   : not found");

  (* [_res] variant where you want to handle failure explicitly. *)
  (match search_users_res conn ~organization_id:id ~email_pattern:"%@example.com" ~limit:10 with
   | Ok rows -> Printf.printf "search : %d row(s), first = %s\n" (List.length rows) (List.hd rows).email
   | Error e -> Printf.printf "search : failed: %s\n" (Sqlml.Error.to_string e));

  (* Nullability from the LEFT JOIN, and the "!"-pinned computed column. *)
  List.iter
    (fun r ->
      Printf.printf "posts  : %-20s title=%-8s count=%d\n" r.email
        (Option.value r.title ~default:"NULL")
        r.post_count)
    (count_posts_by_user conn);

  Printf.printf "deleted: %d\n" (delete_user conn ~id);

  (* The query modules are still exported, so generic code works. Nothing in an
     application needs this -- it is for tooling, tracing, batch runners. *)
  (match Sqlml.fetch_one {Db.Get_user} conn { Db.Get_user.id } with
   | Ok (Some u) -> Printf.printf "generic: %s\n" u.email
   | Ok None -> print_endline "generic: none"
   | Error e -> Printf.printf "generic: failed: %s\n" (Sqlml.Error.to_string e));

  (* And the raising variant really does raise. *)
  match Sqlml.fetch_one {Db.Get_user} conn { Db.Get_user.id } with
  | Error e -> raise (Sqlml.Sql_error e)
  | Ok _ -> print_endline "done"
