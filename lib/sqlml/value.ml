(* See value.mli for the interface story. *)

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
let of_int64 i = Text (Int64.to_string i)
let of_float f = Float f
let of_string s = Text s
let of_octets s = Octets s
let of_option f = function None -> Null | Some v -> f v
let of_uuid u = Text (Uuidm.to_string u)
let of_decimal d = Text (Decimal.to_string d)

(* Postgres accepts RFC3339 on input regardless of its DateStyle setting. *)
let of_ptime t = Text (Ptime.to_rfc3339 ~tz_offset_s:0 t)
let of_json j = Text (Yojson.Safe.to_string j)
let of_date (y, m, d) = Text (Printf.sprintf "%04d-%02d-%02d" y m d)

(* time-of-day as a span since midnight *)
let of_time_of_day span =
  let us = Int64.of_float (Float.round (Ptime.Span.to_float_s span *. 1e6)) in
  let sec = Int64.div us 1_000_000L and frac = Int64.rem us 1_000_000L in
  let sec = Int64.to_int sec in
  Text
    (Printf.sprintf "%02d:%02d:%02d.%06Ld" (sec / 3600)
       (sec mod 3600 / 60)
       (sec mod 60) frac)

let of_interval i = Text (Interval.to_string i)

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
         (* \r \v \f count as whitespace to postgres array_in (scanner_isspace) *)
         | ',' | '{' | '}' | '"' | '\\' | ' ' | '\t' | '\n' | '\r' | '\011' | '\012' ->
             true
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

  let octets s =
    "\\x"
    ^ String.concat ""
        (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

  let int = string_of_int
  let int64 = Int64.to_string
  let float f = Printf.sprintf "%.17g" f
  let bool b = if b then "t" else "f"
  let uuid u = Uuidm.to_string u
  let decimal d = Decimal.to_string d
  let ptime t = Ptime.to_rfc3339 ~tz_offset_s:0 t
  let json j = Yojson.Safe.to_string j
  let date (y, m, d) = Printf.sprintf "%04d-%02d-%02d" y m d
  let time_of_day span = match of_time_of_day span with Text s -> s | _ -> assert false
  let interval i = Interval.to_string i
end

(* [of_list print xs] encodes an array column or parameter. *)
let of_list print xs = Text (array_literal (List.map print xs))

(* The wire encoding both drivers share: PostgreSQL text format, [None] for SQL
   NULL. Living here rather than per driver means the two drivers cannot drift
   apart on how a value is rendered. *)
let to_pg_text = function
  | Null -> None
  | Bool b -> Some (if b then "t" else "f")
  | Int n -> Some (string_of_int n)
  | Float f -> Some (Printf.sprintf "%.17g" f)
  | Text s -> Some s
  | Octets s ->
      let b = Buffer.create ((String.length s * 2) + 2) in
      Buffer.add_string b "\\x";
      String.iter (fun c -> Buffer.add_string b (Printf.sprintf "%02x" (Char.code c))) s;
      Some (Buffer.contents b)

(* COPY text format is its own quoting regime, distinct from array literals:
   backslash-escapes for the control characters the format itself uses as
   structure, and \N for NULL. The server unescapes before type input, so a
   cell is [to_pg_text] first, escaped second. *)
module Copy = struct
  let cell v =
    match to_pg_text v with
    | None -> "\\N"
    | Some s ->
        let b = Buffer.create (String.length s + 8) in
        String.iter
          (fun c ->
            match c with
            | '\\' -> Buffer.add_string b "\\\\"
            | '\b' -> Buffer.add_string b "\\b"
            | '\012' -> Buffer.add_string b "\\f"
            | '\n' -> Buffer.add_string b "\\n"
            | '\r' -> Buffer.add_string b "\\r"
            | '\t' -> Buffer.add_string b "\\t"
            | '\011' -> Buffer.add_string b "\\v"
            | c -> Buffer.add_char b c)
          s;
        Buffer.contents b

  let line vs = String.concat "\t" (List.map cell vs)
end
