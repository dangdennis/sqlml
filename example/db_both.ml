[@@@warning "-69"]

type get_user_row =
  { id : string
  ; email : string
  ; display_name : string option
  }

module Get_user = struct
  type params = { id : string }
  type row = get_user_row

  let name = "GetUser"
  let sql = "SELECT id, email, display_name FROM users WHERE id = $1"
  let encode ({ id } : params) = [ Sqlml.Value.of_string id ]

  let decode r : row =
    { id = Sqlml.Row.string r 0
    ; email = Sqlml.Row.string r 1
    ; display_name = Sqlml.Row.(option string) r 2
    }
end

let get_user conn ~id = Sqlml.fetch_one {Get_user} conn { Get_user.id }
