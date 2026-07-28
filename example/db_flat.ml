(* SHAPE B implementation. The Query modules are local to this file. *)

[@@@warning "-69"]

type user_status =
  | Active
  | Banned

let user_status_to_string = function Active -> "active" | Banned -> "banned"

let user_status_of_row r i =
  match Sqlml.Row.string r i with
  | "active" -> Active
  | "banned" -> Banned
  | other -> raise (Sqlml.Row.Bad { column = i; expected = "user_status"; got = other })

type get_user_row =
  { id : string
  ; email : string
  ; display_name : string option
  ; status : user_status
  ; created_at : string
  }

type search_users_row =
  { id : string
  ; email : string
  ; created_at : string
  }

module Q_get_user = struct
  type params = { id : string }
  type row = get_user_row

  let name = "GetUser"
  let sql = "SELECT id, email, display_name, status, created_at\nFROM users\nWHERE id = $1"
  let encode ({ id } : params) = [ Sqlml.Value.of_string id ]

  let decode r : row =
    { id = Sqlml.Row.string r 0
    ; email = Sqlml.Row.string r 1
    ; display_name = Sqlml.Row.(option string) r 2
    ; status = user_status_of_row r 3
    ; created_at = Sqlml.Row.string r 4
    }
end

let get_user conn ~id = Sqlml.fetch_one {Q_get_user} conn { Q_get_user.id }

module Q_search_users = struct
  type params =
    { organization_id : string
    ; email_pattern : string
    ; limit : int
    }

  type row = search_users_row

  let name = "SearchUsers"

  let sql =
    "SELECT id, email, created_at\n\
     FROM users\n\
     WHERE organization_id = $1\n\
    \  AND email ILIKE $2\n\
     ORDER BY created_at DESC\n\
     LIMIT $3"

  let encode ({ organization_id; email_pattern; limit } : params) =
    Sqlml.Value.[ of_string organization_id; of_string email_pattern; of_int limit ]

  let decode r : row =
    { id = Sqlml.Row.string r 0
    ; email = Sqlml.Row.string r 1
    ; created_at = Sqlml.Row.string r 2
    }
end

let search_users conn ~organization_id ~email_pattern ~limit =
  Sqlml.fetch_all {Q_search_users} conn { Q_search_users.organization_id; email_pattern; limit }

module Q_delete_user = struct
  type params = { id : string }

  let name = "DeleteUser"
  let sql = "DELETE FROM users WHERE id = $1"
  let encode ({ id } : params) = [ Sqlml.Value.of_string id ]
end

let delete_user conn ~id = Sqlml.exec {Q_delete_user} conn { Q_delete_user.id }
