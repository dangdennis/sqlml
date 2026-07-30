(* Interval parsing across the four IntervalStyle output formats PostgreSQL
   can be configured to print, plus the encode format we send back. *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

open Sqlml

let iv ?years ?months ?days ?hours ?minutes ?seconds () =
  Interval.make ?years ?months ?days ?hours ?minutes ?seconds ()

let eq s expected =
  match Interval.of_string_opt s with
  | Some v -> Interval.equal v expected
  | None -> false

let () =
  (* postgres style (the default) *)
  check "postgres: full"
    (eq "1 year 2 mons 3 days 04:05:06.789"
       (Interval.make ~years:1 ~months:2 ~days:3 ~hours:4 ~minutes:5 ~seconds:6
          ~micros:789_000L ()));
  check "postgres: clock only" (eq "04:05:06" (iv ~hours:4 ~minutes:5 ~seconds:6 ()));
  check "postgres: zero" (eq "00:00:00" Interval.zero);
  check "postgres: negative clock"
    (eq "-04:05:06" (iv ~hours:(-4) ~minutes:(-5) ~seconds:(-6) ()));
  check "postgres: interior signs"
    (eq "-1 mons +2 days -03:00:00" (iv ~months:(-1) ~days:2 ~hours:(-3) ()));

  (* postgres_verbose *)
  check "verbose: ago negates" (eq "@ 1 year 2 mons ago" (iv ~years:(-1) ~months:(-2) ()));
  check "verbose: plain" (eq "@ 3 days 4 hours" (iv ~days:3 ~hours:4 ()));

  (* sql_standard *)
  check "sql_standard: year-month" (eq "+1-2" (iv ~years:1 ~months:2 ()));
  check "sql_standard: full"
    (eq "+1-2 +3 +04:05:06"
       (iv ~years:1 ~months:2 ~days:3 ~hours:4 ~minutes:5 ~seconds:6 ()));
  check "sql_standard: negative year-month" (eq "-1-2" (iv ~years:(-1) ~months:(-2) ()));

  (* iso_8601 *)
  check "iso: full"
    (eq "P1Y2M3DT4H5M6.789S"
       (Interval.make ~years:1 ~months:2 ~days:3 ~hours:4 ~minutes:5 ~seconds:6
          ~micros:789_000L ()));
  check "iso: time only" (eq "PT0S" Interval.zero);
  check "iso: negative component" (eq "P-1M2D" (iv ~months:(-1) ~days:2 ()));
  check "iso: weeks" (eq "P2W" (iv ~days:14 ()));

  (* what we send to postgres *)
  check "encode is unit-spelled"
    (Interval.to_string (iv ~months:14 ~days:3 ~hours:4 ())
    = "14 mons 3 days 14400000000 microseconds");
  check "garbage is rejected" (Interval.of_string_opt "not an interval" = None);
  check "empty is rejected" (Interval.of_string_opt "" = None);

  print_endline "all good"
