(* Exercises the generator's real output (example/generated/db.ml) against a
   stub driver. Compiling it proves the emitter produces valid OCaml; this
   proves the emitted decoders, encoders and wrappers actually work. *)

open Generated

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  go 0

let uuid_s = "1b4e28ba-2fa1-11d2-883f-0016d3cca427"
let ts = Sqlml.Value.Text "2026-07-28 09:00:00+00"

module Stub = struct
  type conn = { mutable last_params : Sqlml.Value.t list }

  let name = "stub"
  let placeholder n = "$" ^ string_of_int n
  let close _ = ()

  let query c ~sql ~params ~columns:_ =
    c.last_params <- params;
    if contains sql "organization_id, email" then
      (* GetUserFull: all 7 columns of users *)
      Ok
        [
          [|
            Sqlml.Value.Text uuid_s;
            Sqlml.Value.Text uuid_s;
            Sqlml.Value.Text "dennis@example.com";
            Sqlml.Value.Null;
            Sqlml.Value.Text "banned";
            Sqlml.Value.Text "1234.50";
            ts;
          |];
        ]
    else if contains sql "post_count" then
      Ok
        [
          [| Sqlml.Value.Text "a@b.c"; Sqlml.Value.Null; Sqlml.Value.Int 0 |];
          [| Sqlml.Value.Text "c@d.e"; Sqlml.Value.Text "Hello"; Sqlml.Value.Int 3 |];
        ]
    else if contains sql "display_name" then
      Ok
        [
          [|
            Sqlml.Value.Text uuid_s;
            Sqlml.Value.Text "dennis@example.com";
            Sqlml.Value.Text "Dennis";
            Sqlml.Value.Text "active";
            Sqlml.Value.Text "1234.50";
            ts;
          |];
        ]
    else Ok [ [| Sqlml.Value.Text uuid_s; Sqlml.Value.Text "x@y.z"; ts |] ]

  let exec c ~sql:_ ~params =
    c.last_params <- params;
    Ok 1
end

let state = { Stub.last_params = [] }
let conn = Sqlml.Driver.make (module Stub) state
let uuid = Option.get (Uuidm.of_string uuid_s)

let () =
  (* :one, with an enum and a numeric *)
  (match Db.get_user_exn conn ~id:uuid with
  | Some u ->
      check "get_user decodes" (u.Db.email = "dennis@example.com");
      check "enum decodes" (u.Db.status = Db.Active);
      check "numeric decodes" (Decimal.to_string u.Db.balance = "1234.50");
      check "timestamptz decodes" (Ptime.to_year u.Db.created_at = 2026);
      check "uuid decodes" (Uuidm.equal u.Db.id uuid)
  | None -> check "get_user decodes" false);

  (* the shared model type: one helper, two queries *)
  let email_of (u : Db.user_row) = u.Db.email in
  (match Db.get_user_full_exn conn ~id:uuid with
  | Some u ->
      check "shared model decodes" (email_of u = "dennis@example.com");
      check "second enum label" (u.Db.status = Db.Banned)
  | None -> check "shared model decodes" false);

  (* :many, with the ? and ! overrides *)
  let rows = Db.count_posts_by_user_exn conn in
  check "many returns 2" (List.length rows = 2);
  check "? override -> option" ((List.nth rows 0).Db.title = None);
  check "! override -> non-option" ((List.nth rows 1).Db.post_count = 3);

  (* :exec *)
  check "exec returns count" (Db.delete_user_exn conn ~id:uuid = 1);

  (* optional argument omitted -> NULL reaches the driver *)
  ignore (Db.set_display_name_exn conn ~id:uuid ());
  check "omitted optional encodes NULL"
    (match state.Stub.last_params with Sqlml.Value.Null :: _ -> true | _ -> false);

  (* optional argument supplied *)
  ignore (Db.set_display_name_exn conn ~id:uuid ~display_name:"Dennis" ());
  check "supplied optional encodes Text"
    (match state.Stub.last_params with
    | Sqlml.Value.Text "Dennis" :: _ -> true
    | _ -> false);

  (* encoders: uuid goes out as text *)
  ignore (Db.get_user conn ~id:uuid);
  check "uuid encodes"
    (match state.Stub.last_params with
    | [ Sqlml.Value.Text s ] -> s = uuid_s
    | _ -> false);

  print_endline "all good"
