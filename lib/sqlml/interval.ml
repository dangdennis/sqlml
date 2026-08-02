(* See interval.mli for the interface story. *)

type t = { months : int; days : int; micros : int64 }

let zero = { months = 0; days = 0; micros = 0L }

let make ?(years = 0) ?(months = 0) ?(days = 0) ?(hours = 0) ?(minutes = 0) ?(seconds = 0)
    ?(micros = 0L) () =
  {
    months = (years * 12) + months;
    days;
    micros =
      Int64.add micros
        (Int64.mul 1_000_000L (Int64.of_int ((hours * 3600) + (minutes * 60) + seconds)));
  }

let equal a b = a.months = b.months && a.days = b.days && Int64.equal a.micros b.micros

(* PostgreSQL accepts spelled-out units on input in every IntervalStyle. *)
let to_string t =
  Printf.sprintf "%d mons %d days %Ld microseconds" t.months t.days t.micros

let pp fmt t = Format.pp_print_string fmt (to_string t)

(* ---------- parsing ---------- *)

exception Bad_interval of string

let fail s = raise (Bad_interval s)

let micros_of_clock s =
  (* [-]HH:MM[:SS[.ffffff]] *)
  let neg = String.length s > 0 && s.[0] = '-' in
  let s =
    if neg || (String.length s > 0 && s.[0] = '+') then
      String.sub s 1 (String.length s - 1)
    else s
  in
  let parts = String.split_on_char ':' s in
  let sec_of p =
    match String.split_on_char '.' p with
    | [ whole ] -> (int_of_string whole, 0L)
    | [ whole; frac ] ->
        let frac = if String.length frac > 6 then String.sub frac 0 6 else frac in
        let padded = frac ^ String.make (6 - String.length frac) '0' in
        (int_of_string whole, Int64.of_string padded)
    | _ -> fail s
  in
  let h, m, sec, us =
    match parts with
    | [ h; m ] -> (int_of_string h, int_of_string m, 0, 0L)
    | [ h; m; sc ] ->
        let s0, us = sec_of sc in
        (int_of_string h, int_of_string m, s0, us)
    | _ -> fail s
  in
  let total =
    Int64.add (Int64.mul 1_000_000L (Int64.of_int ((h * 3600) + (m * 60) + sec))) us
  in
  if neg then Int64.neg total else total

let is_clock s =
  String.contains s ':'
  && String.for_all
       (fun c -> (c >= '0' && c <= '9') || c = ':' || c = '.' || c = '-' || c = '+')
       s

(* sql_standard year-month: [+-]Y-M *)
let year_month s =
  match
    String.split_on_char '-'
      (if s.[0] = '+' || s.[0] = '-' then String.sub s 1 (String.length s - 1) else s)
  with
  | [ y; m ] when y <> "" && m <> "" && String.for_all (fun c -> c >= '0' && c <= '9') y
    ->
      let v = (int_of_string y * 12) + int_of_string m in
      Some (if s.[0] = '-' then -v else v)
  | _ -> None

let of_iso s =
  (* P[nY][nM][nW][nD][T[nH][nM][nS]], with possibly negative components *)
  let n = String.length s in
  let months = ref 0 and days = ref 0 and micros = ref 0L in
  let i = ref 1 in
  let in_time = ref false in
  while !i < n do
    if s.[!i] = 'T' || s.[!i] = 't' then begin
      in_time := true;
      incr i
    end
    else begin
      let start = !i in
      while
        !i < n
        && (s.[!i] = '-'
           || s.[!i] = '+'
           || s.[!i] = '.'
           || (s.[!i] >= '0' && s.[!i] <= '9'))
      do
        incr i
      done;
      if !i >= n || !i = start then fail s;
      let num = String.sub s start (!i - start) in
      let unit = s.[!i] in
      incr i;
      let as_int () = int_of_string num in
      match (unit, !in_time) with
      | ('Y' | 'y'), _ -> months := !months + (12 * as_int ())
      | ('M' | 'm'), false -> months := !months + as_int ()
      | ('W' | 'w'), _ -> days := !days + (7 * as_int ())
      | ('D' | 'd'), _ -> days := !days + as_int ()
      | ('H' | 'h'), _ ->
          micros :=
            Int64.add !micros (Int64.mul 3_600_000_000L (Int64.of_int (as_int ())))
      | ('M' | 'm'), true ->
          micros := Int64.add !micros (Int64.mul 60_000_000L (Int64.of_int (as_int ())))
      | ('S' | 's'), _ ->
          let sec = float_of_string num in
          micros := Int64.add !micros (Int64.of_float (Float.round (sec *. 1e6)))
      | _ -> fail s
    end
  done;
  { months = !months; days = !days; micros = !micros }

let of_string s =
  let parse s =
    if s = "" then fail s
    else if s.[0] = 'P' || s.[0] = 'p' then of_iso s
    else begin
      (* postgres / postgres_verbose / sql_standard: unit pairs and clock times.
       Verbose adds a leading '@' and a trailing 'ago' meaning negation. *)
      let s =
        if s.[0] = '@' then String.trim (String.sub s 1 (String.length s - 1)) else s
      in
      let toks = String.split_on_char ' ' s |> List.filter (fun t -> t <> "") in
      let toks, negate_all =
        match List.rev toks with
        | "ago" :: rest -> (List.rev rest, true)
        | _ -> (toks, false)
      in
      let months = ref 0 and days = ref 0 and micros = ref 0L in
      let rec go = function
        | [] -> ()
        | tok :: rest -> (
            if is_clock tok then begin
              micros := Int64.add !micros (micros_of_clock tok);
              go rest
            end
            else
              match year_month tok with
              | Some m ->
                  months := !months + m;
                  go rest
              | None -> (
                  match rest with
                  (* sql_standard prints a bare day count: "+1-2 +3 +04:05:06" *)
                  | next :: _ when is_clock next ->
                      days := !days + int_of_string tok;
                      go rest
                  | unit :: rest' -> (
                      let v = int_of_string tok in
                      match String.lowercase_ascii unit with
                      | "year" | "years" | "yr" | "yrs" ->
                          months := !months + (12 * v);
                          go rest'
                      | "mon" | "mons" | "month" | "months" ->
                          months := !months + v;
                          go rest'
                      | "week" | "weeks" ->
                          days := !days + (7 * v);
                          go rest'
                      | "day" | "days" ->
                          days := !days + v;
                          go rest'
                      | "hour" | "hours" ->
                          micros :=
                            Int64.add !micros (Int64.mul 3_600_000_000L (Int64.of_int v));
                          go rest'
                      | "min" | "mins" | "minute" | "minutes" ->
                          micros :=
                            Int64.add !micros (Int64.mul 60_000_000L (Int64.of_int v));
                          go rest'
                      | "sec" | "secs" | "second" | "seconds" ->
                          micros :=
                            Int64.add !micros (Int64.mul 1_000_000L (Int64.of_int v));
                          go rest'
                      | "microsecond" | "microseconds" ->
                          micros := Int64.add !micros (Int64.of_int v);
                          go rest'
                      | _ -> fail s)
                  | [] ->
                      (* trailing bare number: sql_standard day-only output *)
                      days := !days + int_of_string tok))
      in
      go toks;
      if negate_all then
        { months = - !months; days = - !days; micros = Int64.neg !micros }
      else { months = !months; days = !days; micros = !micros }
    end
  in
  (* int_of_string and friends raise Failure; normalise every parse problem to
     Bad_interval so of_string_opt is total *)
  let s = String.trim s in
  try parse s with Bad_interval _ as e -> raise e | _ -> fail s

let of_string_opt s =
  match of_string s with v -> Some v | exception Bad_interval _ -> None
