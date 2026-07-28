(* Postgres type -> OCaml type, plus the decoder and encoder expressions the
   emitter splices into generated code. *)

type t =
  | Int
  | Float
  | Bool
  | String
  | Bytes
  | Uuid
  | Ptime
  | Decimal
  | Enum of string * string list (* ocaml type name, labels in sort order *)
  | Option of t

let rec ocaml_type = function
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "string"
  | Bytes -> "string"
  | Uuid -> "Uuidm.t"
  | Ptime -> "Ptime.t"
  | Decimal -> "Decimal.t"
  | Enum (n, _) -> n
  | Option t -> ocaml_type t ^ " option"

(* A function of type [Sqlml.Row.t -> int -> _] *)
let rec decoder = function
  | Int -> "Sqlml.Row.int"
  | Float -> "Sqlml.Row.float"
  | Bool -> "Sqlml.Row.bool"
  | String -> "Sqlml.Row.string"
  | Bytes -> "Sqlml.Row.octets"
  | Uuid -> "Sqlml.Row.uuid"
  | Ptime -> "Sqlml.Row.ptime"
  | Decimal -> "Sqlml.Row.decimal"
  | Enum (n, _) -> n ^ "_of_row"
  | Option t -> Printf.sprintf "(Sqlml.Row.option %s)" (decoder t)

(* A function of type [_ -> Sqlml.Value.t] *)
let rec encoder = function
  | Int -> "Sqlml.Value.of_int"
  | Float -> "Sqlml.Value.of_float"
  | Bool -> "Sqlml.Value.of_bool"
  | String -> "Sqlml.Value.of_string"
  | Bytes -> "Sqlml.Value.of_octets"
  | Uuid -> "Sqlml.Value.of_uuid"
  | Ptime -> "Sqlml.Value.of_ptime"
  | Decimal -> "Sqlml.Value.of_decimal"
  | Enum (n, _) -> n ^ "_to_value"
  | Option t -> Printf.sprintf "(Sqlml.Value.of_option %s)" (encoder t)

let is_option = function Option _ -> true | _ -> false
let strip_option = function Option t -> t | t -> t

(* int8 rather than int4 shows up more than you would expect -- LIMIT is typed
   bigint, as is count(). OCaml's int is 63-bit on the platforms we target, so
   int8 -> int is lossless here; it is called out because it is a real decision
   and not an oversight. *)
let base_of_pg_name = function
  | "bool" -> Some Bool
  | "int2" | "int4" | "int8" -> Some Int
  | "float4" | "float8" -> Some Float
  | "numeric" -> Some Decimal
  | "text" | "varchar" | "bpchar" | "char" | "name" | "citext" -> Some String
  | "bytea" -> Some Bytes
  | "uuid" -> Some Uuid
  | "timestamp" | "timestamptz" -> Some Ptime
  (* Ptime.of_rfc3339 needs a full date+time, so these stay textual for now *)
  | "date" | "time" | "timetz" | "interval" -> Some String
  | "json" | "jsonb" -> Some String
  | "inet" | "cidr" | "macaddr" | "macaddr8" -> Some String
  | _ -> None

let of_pg ~type_name ~enum_labels ~nullable =
  let base =
    if enum_labels <> [] then Some (Enum (type_name, enum_labels))
    else base_of_pg_name type_name
  in
  Option.map (fun b -> if nullable then Option b else b) base

(* ---------- enum naming ---------- *)

let sanitize s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then
        Buffer.add_char b c
      else Buffer.add_char b '_')
    s;
  Buffer.contents b

(* 'active' -> Active, 'in-progress' -> In_progress *)
let constructor_of_label label =
  let s = sanitize (String.lowercase_ascii label) in
  let s = if s = "" then "Empty" else s in
  let s = if s.[0] >= '0' && s.[0] <= '9' then "N" ^ s else s in
  String.make 1 (Char.uppercase_ascii s.[0]) ^ String.sub s 1 (String.length s - 1)
