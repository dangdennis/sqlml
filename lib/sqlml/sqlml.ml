(* sqlml runtime -- public interface.

   The whole point of this file is the three execution functions below. Each
   takes a generated query module as a *modular explicit* argument, so the
   module you pass determines both the parameter type you must supply and the
   result type you get back:

     val fetch_one : {Q : Query.ONE} -> Driver.t -> Q.params -> (Q.row option, _) result

   With ordinary first-class modules this type is not writable in curried form:
   [Q.params] and [Q.row] would each need to escape as a separate type variable
   threaded through a [with type params = 'p and type row = 'r] constraint, and
   every caller would have to reconstruct that witness. Here they simply project
   out of the module argument.

   Call sites look like:

     let user = Sqlml.fetch_one {Db.Get_user} conn { id = 42 }
     let users = Sqlml.fetch_all {Db.Search_users} conn { org_id; limit = 100 }
     let n = Sqlml.exec {Db.Delete_user} conn { id = 42 }

   The brace argument is written by the code generator in typical use, so the
   explicitness costs the user nothing -- which is exactly why *explicits* are a
   better fit here than implicits. *)

module Value = Value
module Error = Error
module Row = Row
module Driver = Driver
module Query = Query

(* A connection handle.

   This is deliberately NOT parameterised by a phantom region tag. The intent
   was for [transaction] to hand back a distinct [tx conn] so that a nested
   transaction became a type error. It cannot be done here: the tag does not
   generalise through the modular-explicit query functions, and a binding whose
   type contains a modular-explicit arrow cannot carry an explicit polymorphic
   annotation at all -- both ['k.] and [type k.] are rejected with "the
   universal variable would escape its scope" -- so there is no way to force it.

   The bug the tag was meant to prevent is using an outer handle inside a
   transaction body and silently running on a different connection. That is only
   reachable once pooling exists, and the fix there does not need phantom types:
   make the pool a distinct type carrying no query operations, so that obtaining
   a connection at all requires going through [transaction] or
   [with_connection]. *)
type conn = Driver.t

(* Generated code emits a raising wrapper and a [_res] wrapper per query:

     val get_user     : conn -> id:Uuidm.t -> get_user_row option
     val get_user_res : conn -> id:Uuidm.t -> (get_user_row option, Error.t) result

   The raising one is [or_raise] applied to the other. *)
exception Sql_error of Error.t

let or_raise = function Ok v -> v | Error e -> raise (Sql_error e)

let () =
  Printexc.register_printer (function
    | Sql_error e -> Some ("Sqlml.Sql_error: " ^ Error.to_string e)
    | _ -> None)

let decode_fail name (e : exn) =
  match e with
  | Row.Bad { column; expected; got } -> Error.Decode { query = name; column; expected; got }
  | e -> raise e

let run_query (conn : conn) ~name ~sql ~params ~columns =
  match conn with
  | Driver.Conn ((module D), c) -> (
    match D.query c ~sql ~params ~columns with
    | Ok rows -> Ok rows
    | Error message -> Error (Error.Execute { query = name; sql; message }))

let fetch_all {Q : Query.MANY} (conn : conn) (p : Q.params) : (Q.row list, Error.t) result =
  match run_query conn ~name:Q.name ~sql:Q.sql ~params:(Q.encode p) ~columns:Q.columns with
  | Error e -> Error e
  | Ok rows -> (
    try Ok (List.map Q.decode rows) with e -> Error (decode_fail Q.name e))

let fetch_one {Q : Query.ONE} (conn : conn) (p : Q.params) : (Q.row option, Error.t) result =
  match run_query conn ~name:Q.name ~sql:Q.sql ~params:(Q.encode p) ~columns:Q.columns with
  | Error e -> Error e
  | Ok [] -> Ok None
  | Ok [ r ] -> ( try Ok (Some (Q.decode r)) with e -> Error (decode_fail Q.name e))
  | Ok rows ->
    Error (Error.Cardinality { query = Q.name; expected = "at most 1"; got = List.length rows })

let exec {Q : Query.EXEC} (conn : conn) (p : Q.params) : (int, Error.t) result =
  match conn with
  | Driver.Conn ((module D), c) -> (
    match D.exec c ~sql:Q.sql ~params:(Q.encode p) with
    | Ok n -> Ok n
    | Error message -> Error (Error.Execute { query = Q.name; sql = Q.sql; message }))

(* ---------- transactions ---------- *)

let statement (conn : conn) sql =
  match conn with
  | Driver.Conn ((module D), c) -> (
    match D.exec c ~sql ~params:[] with
    | Ok _ -> Ok ()
    | Error message -> Error (Error.Execute { query = "transaction"; sql; message }))

(* Commits when [f] returns [Ok], rolls back when it returns [Error] or raises.
   An exception is re-raised after the rollback, so [_exn] query functions work
   inside the body and abort the transaction as you would expect. *)
let transaction (conn : conn) f =
  match statement conn "BEGIN" with
  | Error e -> Error e
  | Ok () -> (
    let tx = conn in
    match f tx with
    | Ok v -> (match statement conn "COMMIT" with Ok () -> Ok v | Error e -> Error e)
    | Error e ->
      ignore (statement conn "ROLLBACK");
      Error e
    | exception e ->
      ignore (statement conn "ROLLBACK");
      raise e)
