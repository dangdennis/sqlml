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
