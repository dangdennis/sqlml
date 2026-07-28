(* The wire-neutral value type exchanged with drivers.

   Deliberately small. Richer Postgres types (uuid, timestamptz, json, arrays,
   enums) are carried as [Text] or [Octets] and converted by generated code via
   [Row], so that adding a type mapping never requires a driver change. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | Text of string
  | Octets of string

let type_name = function
  | Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | Text _ -> "text"
  | Octets _ -> "octets"

let of_bool b = Bool b
let of_int i = Int i
let of_float f = Float f
let of_string s = Text s
let of_octets s = Octets s
let of_option f = function None -> Null | Some v -> f v
let of_uuid u = Text (Uuidm.to_string u)
let of_decimal d = Text (Decimal.to_string d)

(* Postgres accepts RFC3339 on input regardless of its DateStyle setting. *)
let of_ptime t = Text (Ptime.to_rfc3339 ~tz_offset_s:0 t)
