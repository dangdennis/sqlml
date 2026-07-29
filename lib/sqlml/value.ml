(** The wire-neutral value type exchanged with drivers.

    Deliberately small. Richer Postgres types (uuid, timestamptz, json, arrays, enums) are
    carried as [Text] or [Octets] and converted by generated code via [Row], so that
    adding a type mapping never requires a driver change. *)

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
let of_json j = Text (Yojson.Safe.to_string j)

(* ---------- arrays ----------

   Postgres reads and writes arrays as "{a,b,c}". An element needs quoting if it
   is empty, contains a delimiter, brace, quote, backslash or whitespace, or
   would otherwise be read as the literal NULL. *)

let needs_quoting s =
  s = ""
  || String.lowercase_ascii s = "null"
  || String.exists
       (fun c ->
         match c with
         | ',' | '{' | '}' | '"' | '\\' | ' ' | '\t' | '\n' -> true
         | _ -> false)
       s

let quote_element s =
  if not (needs_quoting s) then s
  else begin
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b '"';
    String.iter
      (fun c ->
        if c = '"' || c = '\\' then Buffer.add_char b '\\';
        Buffer.add_char b c)
      s;
    Buffer.add_char b '"';
    Buffer.contents b
  end

let array_literal elements =
  "{" ^ String.concat "," (List.map quote_element elements) ^ "}"

(* Element printers, for arrays. Scalar columns go through [of_*] above; array
   elements need a plain [_ -> string] because they are spliced into a literal. *)
module Print = struct
  let string (s : string) = s
  let octets = string
  let int = string_of_int
  let float f = Printf.sprintf "%.17g" f
  let bool b = if b then "t" else "f"
  let uuid = Uuidm.to_string
  let decimal = Decimal.to_string
  let ptime t = Ptime.to_rfc3339 ~tz_offset_s:0 t
  let json = Yojson.Safe.to_string
end

(* [of_list print xs] encodes an array column or parameter. *)
let of_list print xs = Text (array_literal (List.map print xs))
