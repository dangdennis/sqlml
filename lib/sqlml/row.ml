(* See row.mli for the interface story. *)

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
  | Value.Text s -> (
      try int_of_string s
      with _ -> raise (Bad { column = i; expected = "int"; got = s }))
  | v -> bad i "int" v

let int64 r i =
  match get r i with
  | Value.Int n -> Int64.of_int n
  | Value.Text s -> (
      try Int64.of_string s
      with _ -> raise (Bad { column = i; expected = "int64"; got = s }))
  | v -> bad i "int64" v

let bool r i =
  match get r i with
  | Value.Bool b -> b
  | Value.Text ("t" | "true" | "TRUE" | "y" | "1") -> true
  | Value.Text ("f" | "false" | "FALSE" | "n" | "0") -> false
  | v -> bad i "bool" v

let string r i = match get r i with Value.Text s -> s | v -> bad i "text" v

let parse_octets s =
  let n = String.length s in
  if n >= 2 && String.sub s 0 2 = "\\x" then begin
    if n mod 2 <> 0 then invalid_arg "bytea: odd hex length";
    String.init
      ((n - 2) / 2)
      (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 + (2 * i)) 2)))
  end
  else begin
    let b = Buffer.create n and i = ref 0 in
    while !i < n do
      if s.[!i] <> '\\' then (
        Buffer.add_char b s.[!i];
        incr i)
      else if !i + 1 < n && s.[!i + 1] = '\\' then (
        Buffer.add_char b '\\';
        i := !i + 2)
      else if !i + 3 < n then begin
        let raw = String.sub s (!i + 1) 3 in
        if not (String.for_all (fun c -> c >= '0' && c <= '7') raw) then
          invalid_arg "bytea: invalid octal escape";
        Buffer.add_char b (Char.chr (int_of_string ("0o" ^ raw)));
        i := !i + 4
      end
      else invalid_arg "bytea: incomplete escape"
    done;
    Buffer.contents b
  end

let octets r i =
  match get r i with
  | Value.Octets s -> s
  | Value.Text s -> (
      try parse_octets s
      with _ -> raise (Bad { column = i; expected = "bytea"; got = s }))
  | v -> bad i "octets" v

let float r i =
  match get r i with
  | Value.Float f -> f
  | Value.Int n -> float_of_int n
  | Value.Text s -> (
      try float_of_string s
      with _ -> raise (Bad { column = i; expected = "float"; got = s }))
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
       | '+' | '-' ->
           tz := `At i;
           raise Exit
       | 'Z' | 'z' ->
           tz := `Zulu;
           raise Exit
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

let parse_date s =
  match String.split_on_char '-' s with
  | [ y; m; d ] ->
      let date = (int_of_string y, int_of_string m, int_of_string d) in
      if Ptime.of_date date = None then failwith "date" else date
  | _ -> failwith "date"

let date r i =
  match get r i with
  | Value.Text s -> (
      try parse_date s with _ -> raise (Bad { column = i; expected = "date"; got = s }))
  | v -> bad i "date" v

let parse_time_of_day s =
  (* HH:MM:SS[.ffffff] since midnight *)
  match String.split_on_char ':' s with
  | [ h; m; sec ] -> (
      let sec, us =
        match String.split_on_char '.' sec with
        | [ w ] -> (int_of_string w, 0.)
        | [ w; f ] -> (int_of_string w, float_of_string ("0." ^ f))
        | _ -> failwith "time"
      in
      let total =
        float_of_int ((int_of_string h * 3600) + (int_of_string m * 60) + sec) +. us
      in
      match Ptime.Span.of_float_s total with Some sp -> sp | None -> failwith "time")
  | _ -> failwith "time"

let time_of_day r i =
  match get r i with
  | Value.Text s -> (
      try parse_time_of_day s
      with _ -> raise (Bad { column = i; expected = "time"; got = s }))
  | v -> bad i "time" v

let interval r i =
  match get r i with
  | Value.Text s -> (
      try Interval.of_string s
      with _ -> raise (Bad { column = i; expected = "interval"; got = s }))
  | v -> bad i "interval" v

let json r i =
  match get r i with
  | Value.Text s -> (
      try Yojson.Safe.from_string s
      with _ -> raise (Bad { column = i; expected = "json"; got = s }))
  | v -> bad i "json" v

let decimal r i =
  match get r i with
  | Value.Text s -> (
      try Decimal.of_string s
      with _ -> raise (Bad { column = i; expected = "numeric"; got = s }))
  | Value.Int n -> Decimal.of_int n
  | v -> bad i "numeric" v

(* [Row.custom User_id.of_string r 0] for a column mapped to a user type via
   sqlml.toml. A raising [of_string] becomes a decode error like any other. *)
let custom parse r i =
  match get r i with
  | Value.Text s -> (
      try parse s
      with exn ->
        raise
          (Bad
             { column = i; expected = "custom type: " ^ Printexc.to_string exn; got = s })
      )
  | v -> bad i "custom type" v

(* ---------- arrays ----------

   Postgres hands arrays back as a "{a,b,c}" literal. Elements may be quoted,
   with backslash escapes inside quotes; an unquoted NULL is a null element.
   Only one dimension is supported -- a nested "{{..}}" is reported rather than
   silently flattened. *)

module Elem = struct
  (* Element parsers operate on the raw text of one array element, unlike the
     column decoders above which index into a row. *)
  let string s = s
  let octets = parse_octets
  let int s = int_of_string s
  let int64 s = Int64.of_string s
  let float s = float_of_string s

  let bool s =
    match s with
    | "t" | "true" | "TRUE" -> true
    | "f" | "false" | "FALSE" -> false
    | _ -> failwith "bool"

  let uuid s = match Uuidm.of_string s with Some u -> u | None -> failwith "uuid"
  let decimal s = Decimal.of_string s

  let ptime s =
    match Ptime.of_rfc3339 ~strict:false (normalize_timestamp s) with
    | Ok (t, _, _) -> t
    | Error _ -> failwith "timestamp"

  let json s = Yojson.Safe.from_string s
  let date s = parse_date s
  let time_of_day s = parse_time_of_day s
  let interval s = Interval.of_string s
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
      let v =
        if (not !was_quoted) && String.lowercase_ascii (String.trim raw) = "null" then
          None
        else Some (if !was_quoted then raw else String.trim raw)
      in
      out := v :: !out;
      Buffer.clear buf;
      was_quoted := false
    in
    while !i < len do
      let c = inner.[!i] in
      if !quoted then
        begin if c = '\\' && !i + 1 < len then (
          Buffer.add_char buf inner.[!i + 1];
          i := !i + 2)
        else if c = '"' then (
          quoted := false;
          incr i)
        else (
          Buffer.add_char buf c;
          incr i)
        end
      else if c = '"' then (
        quoted := true;
        was_quoted := true;
        incr i)
      else if c = '{' then bad "multidimensional arrays are not supported"
      else if c = ',' then (
        flush ();
        incr i)
      else (
        Buffer.add_char buf c;
        incr i)
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
        | None ->
            raise (Bad { column = i; expected = "non-null array element"; got = "NULL" })
        | Some e -> (
            try parse e
            with _ -> raise (Bad { column = i; expected = "array element"; got = e })))
  | v -> bad i "array" v
