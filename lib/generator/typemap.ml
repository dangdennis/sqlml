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
  | Array of t
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
  | Array t -> ocaml_type t ^ " list"
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
  | Array t -> Printf.sprintf "(Sqlml.Row.list %s)" (elem_parser t)
  | Option t -> Printf.sprintf "(Sqlml.Row.option %s)" (decoder t)

(* Array elements arrive as raw text, so they need [string -> _] parsers rather
   than the row-indexing decoders above. *)
and elem_parser = function
  | Int -> "Sqlml.Row.Elem.int"
  | Float -> "Sqlml.Row.Elem.float"
  | Bool -> "Sqlml.Row.Elem.bool"
  | String | Bytes -> "Sqlml.Row.Elem.string"
  | Uuid -> "Sqlml.Row.Elem.uuid"
  | Ptime -> "Sqlml.Row.Elem.ptime"
  | Decimal -> "Sqlml.Row.Elem.decimal"
  | Enum (n, _) -> n ^ "_of_string"
  | Array _ -> "(fun _ -> failwith \"nested arrays are not supported\")"
  | Option t -> elem_parser t

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
  | Array t -> Printf.sprintf "(Sqlml.Value.of_list %s)" (elem_printer t)
  | Option t -> Printf.sprintf "(Sqlml.Value.of_option %s)" (encoder t)

and elem_printer = function
  | Int -> "Sqlml.Value.Print.int"
  | Float -> "Sqlml.Value.Print.float"
  | Bool -> "Sqlml.Value.Print.bool"
  | String | Bytes -> "Sqlml.Value.Print.string"
  | Uuid -> "Sqlml.Value.Print.uuid"
  | Ptime -> "Sqlml.Value.Print.ptime"
  | Decimal -> "Sqlml.Value.Print.decimal"
  | Enum (n, _) -> n ^ "_to_string"
  | Array _ -> "(fun _ -> failwith \"nested arrays are not supported\")"
  | Option t -> elem_printer t

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

let scalar_of name labels =
  if labels <> [] then Some (Enum (name, labels)) else base_of_pg_name name

(* For an array, [type_name] is Postgres's internal array name (_text) and
   [elem_type_name] is what we actually map; enum labels belong to the element. *)
let of_pg ~type_name ~elem_type_name ~enum_labels ~nullable =
  let base =
    match elem_type_name with
    | Some e -> Option.map (fun x -> Array x) (scalar_of e enum_labels)
    | None -> scalar_of type_name enum_labels
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
