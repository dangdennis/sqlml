(* See typemap.mli. *)

type custom = { c_ocaml : string; c_of_string : string; c_to_string : string }

type t =
  | Int
  | Int64
  | Float
  | Bool
  | String
  | Bytes
  | Uuid
  | Ptime
  | Decimal
  | Json
  | Date
  | Time_of_day
  | Interval
  | Enum of string * string list (* ocaml type name, labels in sort order *)
  | Custom of custom (* from sqlml.toml *)
  | Array of t * char
  | Composite of string
  | Range of t
  | Multirange of t
  | Option of t

let rec ocaml_type = function
  | Int -> "int"
  | Int64 -> "int64"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "string"
  | Bytes -> "string"
  | Uuid -> "Uuidm.t"
  | Ptime -> "Ptime.t"
  | Decimal -> "Decimal.t"
  | Json -> "Yojson.Safe.t"
  | Date -> "Ptime.date"
  | Time_of_day -> "Ptime.Span.t"
  | Interval -> "Sqlml.Interval.t"
  | Enum (n, _) -> n
  | Custom c -> c.c_ocaml
  | Array (t, _) -> "(" ^ ocaml_type t ^ ") Sqlml.Pg_array.t"
  | Composite n -> n
  | Range t -> "(" ^ ocaml_type t ^ ") Sqlml.Range.t"
  | Multirange t -> "(" ^ ocaml_type t ^ ") Sqlml.Range.t list"
  | Option t -> ocaml_type t ^ " option"

(* A function of type [Sqlml.Row.t -> int -> _] *)
let rec decoder = function
  | Int -> "Sqlml.Row.int"
  | Int64 -> "Sqlml.Row.int64"
  | Float -> "Sqlml.Row.float"
  | Bool -> "Sqlml.Row.bool"
  | String -> "Sqlml.Row.string"
  | Bytes -> "Sqlml.Row.octets"
  | Uuid -> "Sqlml.Row.uuid"
  | Ptime -> "Sqlml.Row.ptime"
  | Decimal -> "Sqlml.Row.decimal"
  | Json -> "Sqlml.Row.json"
  | Date -> "Sqlml.Row.date"
  | Time_of_day -> "Sqlml.Row.time_of_day"
  | Interval -> "Sqlml.Row.interval"
  | Enum (n, _) -> n ^ "_of_row"
  | Custom c -> Printf.sprintf "(Sqlml.Row.custom %s)" c.c_of_string
  | (Array _ | Composite _ | Range _ | Multirange _) as t ->
      Printf.sprintf "(Sqlml.Row.custom %s)" (elem_parser t)
  | Option t -> Printf.sprintf "(Sqlml.Row.option %s)" (decoder t)

(* Array elements arrive as raw text, so they need [string -> _] parsers rather
   than the row-indexing decoders above. *)
and elem_parser = function
  | Int -> "Sqlml.Row.Elem.int"
  | Int64 -> "Sqlml.Row.Elem.int64"
  | Float -> "Sqlml.Row.Elem.float"
  | Bool -> "Sqlml.Row.Elem.bool"
  | String -> "Sqlml.Row.Elem.string"
  | Bytes -> "Sqlml.Row.Elem.octets"
  | Uuid -> "Sqlml.Row.Elem.uuid"
  | Ptime -> "Sqlml.Row.Elem.ptime"
  | Decimal -> "Sqlml.Row.Elem.decimal"
  | Json -> "Sqlml.Row.Elem.json"
  | Date -> "Sqlml.Row.Elem.date"
  | Time_of_day -> "Sqlml.Row.Elem.time_of_day"
  | Interval -> "Sqlml.Row.Elem.interval"
  | Enum (n, _) -> n ^ "_of_string"
  | Custom c -> c.c_of_string
  | Array (t, delimiter) ->
      Printf.sprintf "(Sqlml.Pg_array.of_string ~delimiter:%C %s)" delimiter
        (elem_parser t)
  | Composite n -> n ^ "_of_string"
  | Range t -> Printf.sprintf "(Sqlml.Range.of_string %s)" (elem_parser t)
  | Multirange t -> Printf.sprintf "(Sqlml.Range.multirange_of_string %s)" (elem_parser t)
  | Option t -> elem_parser t

(* A function of type [_ -> Sqlml.Value.t] *)
let rec encoder = function
  | Int -> "Sqlml.Value.of_int"
  | Int64 -> "Sqlml.Value.of_int64"
  | Float -> "Sqlml.Value.of_float"
  | Bool -> "Sqlml.Value.of_bool"
  | String -> "Sqlml.Value.of_string"
  | Bytes -> "Sqlml.Value.of_octets"
  | Uuid -> "Sqlml.Value.of_uuid"
  | Ptime -> "Sqlml.Value.of_ptime"
  | Decimal -> "Sqlml.Value.of_decimal"
  | Json -> "Sqlml.Value.of_json"
  | Date -> "Sqlml.Value.of_date"
  | Time_of_day -> "Sqlml.Value.of_time_of_day"
  | Interval -> "Sqlml.Value.of_interval"
  | Enum (n, _) -> n ^ "_to_value"
  | Custom c -> Printf.sprintf "(fun x -> Sqlml.Value.of_string (%s x))" c.c_to_string
  | (Array _ | Composite _ | Range _ | Multirange _) as t ->
      Printf.sprintf "(fun x -> Sqlml.Value.of_string (%s x))" (elem_printer t)
  | Option t -> Printf.sprintf "(Sqlml.Value.of_option %s)" (encoder t)

and elem_printer = function
  | Int -> "Sqlml.Value.Print.int"
  | Int64 -> "Sqlml.Value.Print.int64"
  | Float -> "Sqlml.Value.Print.float"
  | Bool -> "Sqlml.Value.Print.bool"
  | String -> "Sqlml.Value.Print.string"
  | Bytes -> "Sqlml.Value.Print.octets"
  | Uuid -> "Sqlml.Value.Print.uuid"
  | Ptime -> "Sqlml.Value.Print.ptime"
  | Decimal -> "Sqlml.Value.Print.decimal"
  | Json -> "Sqlml.Value.Print.json"
  | Date -> "Sqlml.Value.Print.date"
  | Time_of_day -> "Sqlml.Value.Print.time_of_day"
  | Interval -> "Sqlml.Value.Print.interval"
  | Enum (n, _) -> n ^ "_to_string"
  | Custom c -> c.c_to_string
  | Array (t, delimiter) ->
      Printf.sprintf "(Sqlml.Pg_array.to_string ~delimiter:%C %s)" delimiter
        (elem_printer t)
  | Composite n -> n ^ "_to_string"
  | Range t -> Printf.sprintf "(Sqlml.Range.to_string %s)" (elem_printer t)
  | Multirange t ->
      Printf.sprintf "(Sqlml.Range.multirange_to_string %s)" (elem_printer t)
  | Option t -> elem_printer t

let is_option = function Option _ -> true | _ -> false
let strip_option = function Option t -> t | t -> t

(* int8 spans the full signed 64-bit PostgreSQL range. *)
let base_of_pg_name = function
  | "bool" -> Some Bool
  | "int2" | "int4" -> Some Int
  | "int8" -> Some Int64
  | "float4" | "float8" -> Some Float
  | "numeric" -> Some Decimal
  | "text" | "varchar" | "bpchar" | "char" | "name" | "citext" -> Some String
  | "bytea" -> Some Bytes
  | "uuid" -> Some Uuid
  | "timestamp" | "timestamptz" -> Some Ptime
  | "date" -> Some Date
  | "time" -> Some Time_of_day
  | "interval" -> Some Interval
  (* timetz is a type even the PostgreSQL docs discourage; it stays textual *)
  | "timetz" -> Some String
  | "json" | "jsonb" -> Some Json
  | "inet" | "cidr" | "macaddr" | "macaddr8" -> Some String
  | _ -> None

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

let of_type ?(custom = fun _ -> None) ?(rename = Pg_type.generated_name) pg ~nullable =
  let rec map seen pg =
    match custom pg.Pg_type.id with
    | Some c -> Some (Custom c)
    | None -> (
        if List.mem pg.id seen then None
        else
          let child id = map (pg.id :: seen) (Pg_type.at pg id) in
          match Pg_type.kind pg with
          | Pg_type.Base ->
              if pg.id.schema = "pg_catalog" || pg.id.name = "citext" then
                base_of_pg_name pg.id.name
              else None
          | Pg_type.Enum labels -> Some (Enum (rename pg.id, labels))
          | Pg_type.Domain d -> child d.base
          | Pg_type.Array a ->
              Option.map (fun x -> Array (x, a.delimiter)) (child a.element)
          | Pg_type.Composite _ -> Some (Composite (rename pg.id))
          | Pg_type.Range r -> Option.map (fun t -> Range t) (child r)
          | Pg_type.Multirange r -> (
              match Pg_type.kind (Pg_type.at pg r) with
              | Pg_type.Range sub -> Option.map (fun t -> Multirange t) (child sub)
              | _ -> None)
          | Pg_type.Unsupported _ -> None)
  in
  Option.map (fun t -> if nullable then Option t else t) (map [] pg)
