(* Hand-written stand-in for generator output, plus an in-memory driver, so the
   typing story is exercised with no database in the loop. Once bin/ emits real
   modules, this file becomes the shape those modules must match. *)

(* Generated files must carry this: a row record's fields are written by the
   decoder but often only some are read by the caller, which trips warning 69
   in the *defining* module -- i.e. in generated code, for a reason the user
   cannot fix. *)
[@@@warning "-69"]

(* ---------- a fake driver ---------- *)

module Fake = struct
  type conn = {
    mutable last : string * Sqlml.Value.t list;
    rows : Sqlml.Value.t array list;
  }

  let close _ = ()

  let query c ~sql ~params ~columns:_ =
    c.last <- (sql, params);
    Ok c.rows

  let exec c ~sql ~params =
    c.last <- (sql, params);
    Ok (List.length c.rows)
end

let conn rows = Sqlml.Driver.make (module Fake) { Fake.last = ("", []); rows }

(* ---------- what the generator will emit ---------- *)

(* -- name: GetUser :one
   SELECT id, email, display_name FROM users WHERE id = $1; *)
module Get_user = struct
  type params = { id : int }
  type row = { id : int; email : string; display_name : string option }

  let name = "GetUser"
  let sql (_ : params) = "SELECT id, email, display_name FROM users WHERE id = $1"

  (* annotation required: [row] also has an [id] field and is defined later, so
     an unannotated [{ id }] pattern would resolve to [row]. The generator must
     emit this annotation on every encode. *)
  let encode ({ id } : params) = [ Sqlml.Value.of_int id ]

  let decode r : row =
    {
      id = Sqlml.Row.int r 0;
      email = Sqlml.Row.string r 1;
      display_name = Sqlml.Row.(option string) r 2;
    }

  let columns = 3
  let cardinality = Sqlml.Query.One
end

(* -- name: SearchUsers :many *)
module Search_users = struct
  type params = { pattern : string; limit : int }
  type row = { id : int; email : string }

  let name = "SearchUsers"
  let sql (_ : params) = "SELECT id, email FROM users WHERE email ILIKE $1 LIMIT $2"
  let encode { pattern; limit } = Sqlml.Value.[ of_string pattern; of_int limit ]
  let decode r = { id = Sqlml.Row.int r 0; email = Sqlml.Row.string r 1 }
  let columns = 2
  let cardinality = Sqlml.Query.Many
end

(* -- name: DeleteUser :exec *)
module Delete_user = struct
  type params = { id : int }

  let name = "DeleteUser"
  let sql (_ : params) = "DELETE FROM users WHERE id = $1"
  let encode { id } = [ Sqlml.Value.of_int id ]
  let cardinality = Sqlml.Query.Exec
end

(* ---------- the part that matters ---------- *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

let user_row id email display_name =
  [|
    Sqlml.Value.Int id;
    Sqlml.Value.Text email;
    (match display_name with None -> Sqlml.Value.Null | Some s -> Sqlml.Value.Text s);
  |]

let () =
  (* fetch_one: the module argument fixes params to Get_user.params and the
     result to Get_user.row option. Both are inferred here -- no annotations. *)
  let db = conn [ user_row 42 "a@example.com" (Some "Dennis") ] in
  (match Sqlml.fetch_one (module Get_user) db { id = 42 } with
  | Ok (Some u) ->
      check "fetch_one decodes"
        (u.Get_user.id = 42 && u.Get_user.display_name = Some "Dennis")
  | Ok None -> check "fetch_one decodes" false
  | Error e ->
      print_endline (Sqlml.Error.to_string e);
      exit 1);

  (* NULL flows into the option field *)
  let db = conn [ user_row 7 "b@example.com" None ] in
  (match Sqlml.fetch_one (module Get_user) db { id = 7 } with
  | Ok (Some u) -> check "null -> None" (u.Get_user.display_name = None)
  | _ -> check "null -> None" false);

  (* :one with more than one row is an error, not a silent truncation *)
  let db = conn [ user_row 1 "x@y.z" None; user_row 2 "p@q.r" None ] in
  (match Sqlml.fetch_one (module Get_user) db { id = 1 } with
  | Error (Sqlml.Error.Cardinality _) -> check "one rejects 2 rows" true
  | _ -> check "one rejects 2 rows" false);

  (* fetch_all *)
  let db = conn [ [| Sqlml.Value.Int 1; Sqlml.Value.Text "a@b.c" |] ] in
  (match Sqlml.fetch_all (module Search_users) db { pattern = "%@b.c"; limit = 10 } with
  | Ok [ u ] -> check "fetch_all decodes" (u.Search_users.email = "a@b.c")
  | _ -> check "fetch_all decodes" false);

  (* a type error in the decoder surfaces as Error.Decode, not an exception *)
  let db = conn [ [| Sqlml.Value.Text "not-an-int"; Sqlml.Value.Text "a@b.c" |] ] in
  (match Sqlml.fetch_all (module Search_users) db { pattern = "%"; limit = 10 } with
  | Error (Sqlml.Error.Decode { column = 0; _ }) -> check "decode error is caught" true
  | _ -> check "decode error is caught" false);

  (* exec *)
  let db = conn [ user_row 42 "a@b.c" None ] in
  (match Sqlml.exec (module Delete_user) db { id = 42 } with
  | Ok n -> check "exec returns count" (n = 1)
  | Error _ -> check "exec returns count" false);

  (* ---------- SQL sequences the runtime emits ----------

     A recording driver captures every statement, so transaction nesting,
     retry, and cursor plumbing are asserted as the exact SQL conversation --
     no database required. *)
  let module Rec = struct
    type conn = {
      mutable log : string list;
      (* fail the next statement with this prefix, once *)
      mutable fail_on : (string * Sqlml.Driver.error) option;
    }

    let close _ = ()

    let starts_with p s =
      String.length s >= String.length p && String.sub s 0 (String.length p) = p

    let take c sql =
      c.log <- sql :: c.log;
      match c.fail_on with
      | Some (prefix, e) when starts_with prefix sql ->
          c.fail_on <- None;
          Some e
      | _ -> None

    let query c ~sql ~params:_ ~columns:_ =
      match take c sql with Some e -> Error e | None -> Ok []

    let exec c ~sql ~params:_ = match take c sql with Some e -> Error e | None -> Ok 1
  end in
  let fresh () = { Rec.log = []; fail_on = None } in
  let conn_of c = Sqlml.Driver.make (module Rec) c in
  let log c = List.rev c.Rec.log in
  let starts_with p s =
    String.length s >= String.length p && String.sub s 0 (String.length p) = p
  in

  (* plain transaction: BEGIN, body, COMMIT *)
  let c = fresh () in
  let conn = conn_of c in
  ignore
    (Sqlml.transaction conn (fun tx -> Sqlml.exec (module Delete_user) tx { id = 1 }));
  check "transaction sequence"
    (log c = [ "BEGIN"; "DELETE FROM users WHERE id = $1"; "COMMIT" ]);

  (* error body: ROLLBACK, not COMMIT *)
  let c = fresh () in
  ignore (Sqlml.transaction (conn_of c) (fun _ -> Error (Sqlml.Error.Connect "no")));
  check "error rolls back" (log c = [ "BEGIN"; "ROLLBACK" ]);

  (* nesting: savepoints with depth-derived names *)
  let c = fresh () in
  let conn = conn_of c in
  ignore
    (Sqlml.transaction conn (fun tx ->
         Sqlml.transaction tx (fun _ -> Error (Sqlml.Error.Connect "inner")) |> ignore;
         Ok ()));
  check "savepoint sequence"
    (log c
    = [
        "BEGIN";
        "SAVEPOINT sqlml_savepoint_1";
        "ROLLBACK TO SAVEPOINT sqlml_savepoint_1";
        "COMMIT";
      ]);

  (* isolation prefix *)
  let c = fresh () in
  ignore (Sqlml.transaction ~isolation:`Serializable (conn_of c) (fun _ -> Ok ()));
  check "isolation in BEGIN" (log c = [ "BEGIN ISOLATION LEVEL SERIALIZABLE"; "COMMIT" ]);

  (* retry: a 40001 body failure re-runs the whole transaction once *)
  let c = fresh () in
  let serialization =
    Sqlml.Driver.error ~sqlstate:(Sqlml.Sqlstate.of_string "40001") "boom"
  in
  c.Rec.fail_on <- Some ("DELETE", serialization);
  (match
     Sqlml.transaction ~retry:2 (conn_of c) (fun tx ->
         Sqlml.exec (module Delete_user) tx { id = 1 })
   with
  | Ok _ ->
      check "retry re-runs after 40001"
        (log c
        = [
            "BEGIN";
            "DELETE FROM users WHERE id = $1";
            "ROLLBACK";
            "BEGIN";
            "DELETE FROM users WHERE id = $1";
            "COMMIT";
          ])
  | Error _ -> check "retry re-runs after 40001" false);

  (* fetch_fold: DECLARE/FETCH/CLOSE inside a transaction, and the cursor name
     is deterministic -- two calls, identical statements, no cache growth *)
  let run_fold c =
    ignore
      (Sqlml.fetch_fold
         (module Search_users)
         ~batch:7 (conn_of c) { pattern = "%"; limit = 1 } ~init:0
         ~f:(fun n _ -> n + 1))
  in
  let c = fresh () in
  run_fold c;
  let first = log c in
  check "cursor conversation shape"
    (match first with
    | [ "BEGIN"; declare; fetch; close_; "COMMIT" ] ->
        starts_with "DECLARE sqlml_cursor_d1 NO SCROLL CURSOR FOR" declare
        && fetch = "FETCH FORWARD 7 FROM sqlml_cursor_d1"
        && close_ = "CLOSE sqlml_cursor_d1"
    | _ -> false);
  let c2 = fresh () in
  run_fold c2;
  check "cursor names are deterministic across calls" (log c2 = first);

  print_endline "all good"
