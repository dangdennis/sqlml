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

type conn = Driver.t

let decode_fail name (e : exn) =
  match e with
  | Row.Bad { column; expected; got } -> Error.Decode { query = name; column; expected; got }
  | e -> raise e

let run_query (conn : conn) ~name ~sql ~params =
  match conn with
  | Driver.Conn ((module D), c) -> (
    match D.query c ~sql ~params with
    | Ok rows -> Ok rows
    | Error message -> Error (Error.Execute { query = name; sql; message }))

let fetch_all {Q : Query.MANY} (conn : conn) (p : Q.params) : (Q.row list, Error.t) result =
  match run_query conn ~name:Q.name ~sql:Q.sql ~params:(Q.encode p) with
  | Error e -> Error e
  | Ok rows -> (
    try Ok (List.map Q.decode rows) with e -> Error (decode_fail Q.name e))

let fetch_one {Q : Query.ONE} (conn : conn) (p : Q.params) : (Q.row option, Error.t) result =
  match run_query conn ~name:Q.name ~sql:Q.sql ~params:(Q.encode p) with
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
