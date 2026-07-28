(* Side-by-side call sites for the two candidate generated-API shapes.
   This is hand-written to stand in for generator output so the ergonomics can
   be judged from real, compiling code rather than a sketch. *)

(* A stub driver so the example actually runs without a database. *)
module Stub = struct
  type conn = unit

  let name = "stub"
  let placeholder n = "$" ^ string_of_int n

  let row =
    [| Sqlml.Value.Text "u-1"
     ; Sqlml.Value.Text "dennis@example.com"
     ; Sqlml.Value.Null
     ; Sqlml.Value.Text "active"
     ; Sqlml.Value.Text "2026-07-28T09:00:00Z"
    |]

  let contains hay needle =
    let nh = String.length hay and nn = String.length needle in
    let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
    go 0

  let query () ~sql ~params:_ =
    if contains sql "display_name" then Ok [ row ]
    else if contains sql "SELECT" then
      Ok [ [| Sqlml.Value.Text "u-1"; Sqlml.Value.Text "a@b.c"; Sqlml.Value.Text "2026-07-28" |] ]
    else Ok []

  let exec () ~sql:_ ~params:_ = Ok 1
end

let conn = Sqlml.Driver.make (module Stub) ()

let fail e = prerr_endline (Sqlml.Error.to_string e); exit 1

let () =
  print_endline "== Shape A: nested module per query ==";

  (* The wrapper hides the modular explicit entirely. *)
  (match Db.get_user conn ~id:"u-1" with
   | Ok (Some u) ->
     (* Field access needs the record's module. This is the wart in Shape A. *)
     Printf.printf "  qualified : %s\n" u.Db.Get_user.email;
     (* ...or a local open, which reads better but you write it every time. *)
     let open Db.Get_user in
     Printf.printf "  local open: %s (display_name = %s)\n" u.email
       (match u.display_name with None -> "NULL" | Some s -> s)
   | Ok None -> print_endline "  not found"
   | Error e -> fail e);

  (* Shape A also keeps the query module public, so the generic runtime works
     directly -- useful for tooling, middleware, or anything that wants to be
     polymorphic over queries. Shape B cannot do this. *)
  (match Sqlml.fetch_all {Db.Search_users} conn
           { Db.Search_users.organization_id = "org-1"
           ; email_pattern = "%@example.com"
           ; limit = 10
           }
   with
   | Ok rows -> Printf.printf "  generic  : %d row(s)\n" (List.length rows)
   | Error e -> fail e);

  print_endline "";
  print_endline "== Shape B: flat types, query modules hidden ==";

  (match Db_flat.get_user conn ~id:"u-1" with
   | Ok (Some u) ->
     (* [u.email] resolves by type-directed disambiguation -- no open needed,
        even though search_users_row also has an [email] field. *)
     Printf.printf "  direct   : %s\n" u.Db_flat.email
   | Ok None -> print_endline "  not found"
   | Error e -> fail e);

  (match Db_flat.search_users conn ~organization_id:"org-1" ~email_pattern:"%" ~limit:10 with
   | Ok rows -> Printf.printf "  search   : %d row(s)\n" (List.length rows)
   | Error e -> fail e);

  (match Db_flat.delete_user conn ~id:"u-1" with
   | Ok n -> Printf.printf "  deleted  : %d\n" n
   | Error e -> fail e);

  print_endline "";
  print_endline "== Shape C: flat row types AND exported query modules ==";

  (* One open brings the function and every row's field labels into scope. In a
     real file this is a single [open Db] at the top, then [u.email] anywhere. *)
  Db_both.(
    match get_user conn ~id:"u-1" with
    | Ok (Some u) ->
      Printf.printf "  direct   : %s (display_name = %s)\n" u.email
        (Option.value u.display_name ~default:"NULL")
    | Ok None -> print_endline "  not found"
    | Error e -> fail e);

  (* ...and the generic runtime still works, which Shape B gave up. *)
  (match Sqlml.fetch_one {Db_both.Get_user} conn { Db_both.Get_user.id = "u-1" } with
   | Ok (Some u) -> Printf.printf "  generic  : %s\n" u.Db_both.email
   | _ -> print_endline "  generic  : none")
