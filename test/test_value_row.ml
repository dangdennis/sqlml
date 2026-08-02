(* Codec round-trips with no database: what Value encodes, Row must decode.
   These are the exact string formats PostgreSQL prints and accepts, so a
   change here is a wire-compat change. *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

open Sqlml

let txt = function Value.Text s -> s | _ -> failwith "expected Text"
let row1 v = [| v |]

let () =
  (* ---------- arrays: quoting round-trips through our own writer/reader ---------- *)
  let round_trip xs =
    let lit = txt (Value.of_list Value.Print.string xs) in
    Row.list Row.Elem.string (row1 (Value.Text lit)) 0
  in
  let tricky =
    [ "plain"; "a,b"; "has \"quote\""; "{braces}"; "back\\slash"; "sp ace"; ""; "NULL" ]
  in
  check "array quoting round-trips the classic cases" (round_trip tricky = tricky);
  (* the whitespace postgres treats as such but naive quoting misses *)
  let ws = [ "a\rb"; "a\011b"; "a\012c"; "a\tb"; "a\nb" ] in
  check "array quoting round-trips \\r \\v \\f" (round_trip ws = ws);
  check "empty array" (round_trip [] = []);

  (* a NULL element is an error, never a silent drop *)
  (match Row.list Row.Elem.string (row1 (Value.Text "{a,NULL,b}")) 0 with
  | exception Row.Bad _ -> check "NULL array element rejected" true
  | _ -> check "NULL array element rejected" false);
  (* but the literal string NULL, quoted, is data *)
  check "quoted \"NULL\" is the string"
    (Row.list Row.Elem.string (row1 (Value.Text {|{"NULL"}|})) 0 = [ "NULL" ]);
  (match Row.list Row.Elem.string (row1 (Value.Text "{{1,2},{3}}")) 0 with
  | exception Row.Bad _ -> check "nested arrays rejected" true
  | _ -> check "nested arrays rejected" false);

  (* ---------- timestamps: every shape postgres prints ---------- *)
  let p s = Row.ptime (row1 (Value.Text s)) 0 in
  let instant = p "2026-07-28 09:00:00+00" in
  check "space + 2-digit offset"
    (Ptime.to_rfc3339 ~tz_offset_s:0 instant = "2026-07-28T09:00:00Z");
  check "offset with minutes names the same instant"
    (Ptime.equal (p "2026-07-28 14:30:00+05:30") (p "2026-07-28 09:00:00+00"));
  check "no offset reads as UTC by convention"
    (Ptime.equal (p "2026-07-28 09:00:00") instant);
  check "fractional seconds survive"
    (Ptime.equal
       (p "2026-07-28 09:00:00.500+00")
       (Option.get (Ptime.add_span instant (Option.get (Ptime.Span.of_float_s 0.5)))));
  (match p "yesterday-ish" with
  | exception Row.Bad _ -> check "garbage timestamp rejected" true
  | _ -> check "garbage timestamp rejected" false);

  (* ---------- date / time-of-day ---------- *)
  check "date" (Row.date (row1 (Value.Text "2024-02-29")) 0 = (2024, 2, 29));
  (match Row.date (row1 (Value.Text "2023-02-29")) 0 with
  | exception Row.Bad _ -> check "invalid date rejected" true
  | _ -> check "invalid date rejected" false);
  let tod = Row.time_of_day (row1 (Value.Text "14:30:05.25")) 0 in
  check "time of day to the fraction"
    (Ptime.Span.equal tod
       (Option.get (Ptime.Span.of_float_s ((14. *. 3600.) +. (30. *. 60.) +. 5.25))));
  check "time encodes back" (txt (Value.of_time_of_day tod) = "14:30:05.250000");

  (* ---------- int8 overflow is a decode error, not silent nonsense ---------- *)
  (match Row.int (row1 (Value.Text "9223372036854775807")) 0 with
  | exception Row.Bad _ -> check "int64 max overflows OCaml int loudly" true
  | _ -> check "int64 max overflows OCaml int loudly" false);
  check "63-bit max still fits"
    (Row.int (row1 (Value.Text "4611686018427387903")) 0 = 4611686018427387903);

  (* ---------- bool strictness, scalar and element agree ---------- *)
  check "scalar bool t" (Row.bool (row1 (Value.Text "t")) 0);
  (match Row.list Row.Elem.bool (row1 (Value.Text "{t,x}")) 0 with
  | exception Row.Bad _ -> check "garbage bool element rejected" true
  | _ -> check "garbage bool element rejected" false);

  (* ---------- to_pg_text: NULs must be visible before the driver ---------- *)
  check "to_pg_text NULL" (Value.to_pg_text Value.Null = None);
  check "to_pg_text bool" (Value.to_pg_text (Value.Bool true) = Some "t");
  check "to_pg_text octets hex"
    (Value.to_pg_text (Value.Octets "\x00\xff") = Some "\\x00ff");

  print_endline "all good"
