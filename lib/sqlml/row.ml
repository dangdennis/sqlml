(* Positional row decoders used by generated code.

   Generated decoders read like the SELECT list they came from:

     let decode r =
       { id          = Row.int r 0;
         email       = Row.string r 1;
         created_at  = Row.ptime r 2;
         display_name = Row.(option string) r 3 }

   Decoders raise [Bad] rather than returning a result so generated code stays
   flat; [Exec] catches it and turns it into [Error.Decode]. *)

type t = Value.t array

exception Bad of { column : int; expected : string; got : string }

let bad column expected v = raise (Bad { column; expected; got = Value.type_name v })

let get (r : t) i =
  if i < Array.length r then r.(i)
  else raise (Bad { column = i; expected = "a column"; got = "row of fewer columns" })

(* Text is accepted everywhere a scalar is expected: a driver reading Postgres
   in text mode hands back every column as a string, so parsing belongs here
   rather than being duplicated in each driver. *)
let int r i =
  match get r i with
  | Value.Int n -> n
  | Value.Text s -> (try int_of_string s with _ -> raise (Bad { column = i; expected = "int"; got = s }))
  | v -> bad i "int" v

let bool r i =
  match get r i with
  | Value.Bool b -> b
  | Value.Text ("t" | "true" | "TRUE" | "y" | "1") -> true
  | Value.Text ("f" | "false" | "FALSE" | "n" | "0") -> false
  | v -> bad i "bool" v
let string r i = match get r i with Value.Text s -> s | v -> bad i "text" v
let octets r i = match get r i with Value.Octets s | Value.Text s -> s | v -> bad i "octets" v

let float r i =
  match get r i with
  | Value.Float f -> f
  | Value.Int n -> float_of_int n
  | Value.Text s -> (try float_of_string s with _ -> raise (Bad { column = i; expected = "float"; got = s }))
  | v -> bad i "float" v

(* [option] wraps another decoder: [Row.(option string) r 3] *)
let option decode r i = match get r i with Value.Null -> None | _ -> Some (decode r i)

(* ---------- richer Postgres types ---------- *)

(* Postgres emits ISO timestamps as "2026-07-28 09:00:00+00": a space instead of
   RFC3339's 'T', and a two-digit offset instead of "+00:00". A timestamp
   (without time zone) carries no offset at all. Normalise all three so
   Ptime.of_rfc3339 accepts them. *)
let normalize_timestamp s =
  let s = String.map (fun c -> if c = ' ' then 'T' else c) s in
  let n = String.length s in
  (* Scan back for the offset sign, stopping before the date's own dashes. *)
  let tz = ref `None in
  (try
     for i = n - 1 downto 10 do
       match s.[i] with
       | '+' | '-' -> tz := `At i; raise Exit
       | 'Z' | 'z' -> tz := `Zulu; raise Exit
       | _ -> ()
     done
   with Exit -> ());
  match !tz with
  | `Zulu -> s
  | `None -> s ^ "Z"
  | `At i -> (
    match n - i with
    | 3 -> s ^ ":00" (* +00    *)
    | 5 -> String.sub s 0 (i + 3) ^ ":" ^ String.sub s (i + 3) 2 (* +0000  *)
    | _ -> s (* +00:00 *))

let ptime r i =
  match get r i with
  | Value.Text s -> (
    match Ptime.of_rfc3339 ~strict:false (normalize_timestamp s) with
    | Ok (t, _, _) -> t
    | Error _ -> raise (Bad { column = i; expected = "timestamp"; got = s }))
  | v -> bad i "timestamp" v

let uuid r i =
  match get r i with
  | Value.Text s -> (
    match Uuidm.of_string s with
    | Some u -> u
    | None -> raise (Bad { column = i; expected = "uuid"; got = s }))
  | v -> bad i "uuid" v

let decimal r i =
  match get r i with
  | Value.Text s -> (
    try Decimal.of_string s
    with _ -> raise (Bad { column = i; expected = "numeric"; got = s }))
  | Value.Int n -> Decimal.of_int n
  | v -> bad i "numeric" v

(* ---------- arrays ----------

   Postgres hands arrays back as a "{a,b,c}" literal. Elements may be quoted,
   with backslash escapes inside quotes; an unquoted NULL is a null element.
   Only one dimension is supported -- a nested "{{..}}" is reported rather than
   silently flattened. *)

module Elem = struct
  (* Element parsers operate on the raw text of one array element, unlike the
     column decoders above which index into a row. *)
  let string s = s
  let int s = int_of_string s
  let float s = float_of_string s
  let bool s = match s with "t" | "true" | "TRUE" -> true | _ -> false
  let uuid s = match Uuidm.of_string s with Some u -> u | None -> failwith "uuid"
  let decimal s = Decimal.of_string s
  let ptime s = match Ptime.of_rfc3339 ~strict:false (normalize_timestamp s) with
    | Ok (t, _, _) -> t
    | Error _ -> failwith "timestamp"
end

let parse_array_literal ~column s =
  let bad msg = raise (Bad { column; expected = "array"; got = msg ^ ": " ^ s }) in
  let s = String.trim s in
  if String.length s < 2 || s.[0] <> '{' || s.[String.length s - 1] <> '}' then
    bad "not an array literal";
  let inner = String.sub s 1 (String.length s - 2) in
  if String.trim inner = "" then []
  else begin
    let out = ref [] in
    let buf = Buffer.create 16 in
    let i = ref 0 in
    let len = String.length inner in
    let quoted = ref false in
    let was_quoted = ref false in
    let flush () =
      let raw = Buffer.contents buf in
      let v = if (not !was_quoted) && String.lowercase_ascii (String.trim raw) = "null" then None
              else Some (if !was_quoted then raw else String.trim raw) in
      out := v :: !out;
      Buffer.clear buf;
      was_quoted := false
    in
    while !i < len do
      let c = inner.[!i] in
      if !quoted then begin
        if c = '\\' && !i + 1 < len then (Buffer.add_char buf inner.[!i + 1]; i := !i + 2)
        else if c = '"' then (quoted := false; incr i)
        else (Buffer.add_char buf c; incr i)
      end
      else if c = '"' then (quoted := true; was_quoted := true; incr i)
      else if c = '{' then bad "multidimensional arrays are not supported"
      else if c = ',' then (flush (); incr i)
      else (Buffer.add_char buf c; incr i)
    done;
    flush ();
    List.rev !out
  end

(* [Row.list Elem.int r 2] decodes an array column into a list. A NULL element
   is an error rather than silently dropped: Postgres does not report whether
   elements are nullable, so the honest default is to reject. *)
let list parse r i =
  match get r i with
  | Value.Text s ->
    parse_array_literal ~column:i s
    |> List.map (function
         | None -> raise (Bad { column = i; expected = "non-null array element"; got = "NULL" })
         | Some e -> (
           try parse e
           with _ -> raise (Bad { column = i; expected = "array element"; got = e })))
  | v -> bad i "array" v
