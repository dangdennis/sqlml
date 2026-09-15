type 'a bound = Unbounded | Inclusive of 'a | Exclusive of 'a
type 'a t = Empty | Bounds of { lower : 'a bound; upper : 'a bound }

let read parse s p =
  let n = String.length s in
  if !p + 5 <= n && String.sub s !p 5 = "empty" then (
    p := !p + 5;
    Empty)
  else begin
    if !p >= n || (s.[!p] <> '[' && s.[!p] <> '(') then invalid_arg "range: opening bound";
    let inclusive = s.[!p] = '[' in
    incr p;
    let lo, lq = Pg_text.token s p ~stop:(( = ) ',') in
    if !p >= n || s.[!p] <> ',' then invalid_arg "range: separator";
    incr p;
    let hi, hq = Pg_text.token s p ~stop:(fun c -> c = ']' || c = ')') in
    if !p >= n then invalid_arg "range: closing bound";
    let upper_inclusive = s.[!p] = ']' in
    incr p;
    let bound raw quoted inc =
      if raw = "" && not quoted then Unbounded
      else
        let v = parse raw in
        if inc then Inclusive v else Exclusive v
    in
    Bounds
      {
        lower = Pg_text.context "range lower" (fun () -> bound lo lq inclusive);
        upper = Pg_text.context "range upper" (fun () -> bound hi hq upper_inclusive);
      }
  end

let of_string parse s =
  let s = String.trim s in
  let p = ref 0 in
  let x = read parse s p in
  if !p <> String.length s then invalid_arg "range: trailing text";
  x

let to_string print = function
  | Empty -> "empty"
  | Bounds { lower; upper } -> (
      let text = function
        | Unbounded -> ""
        | Inclusive x | Exclusive x -> Pg_text.quote (print x)
      in
      (match lower with Inclusive _ -> "[" | _ -> "(")
      ^ text lower ^ "," ^ text upper
      ^ match upper with Inclusive _ -> "]" | _ -> ")")

let multirange_of_string parse s =
  let s = String.trim s in
  let n = String.length s in
  if n < 2 || s.[0] <> '{' || s.[n - 1] <> '}' then
    invalid_arg "multirange: expected braces";
  let p = ref 1 in
  if n = 2 then []
  else
    let rec loop acc =
      let x = read parse s p in
      if !p = n - 1 then List.rev (x :: acc)
      else if !p < n - 1 && s.[!p] = ',' then (
        incr p;
        loop (x :: acc))
      else invalid_arg "multirange: separator or trailing text"
    in
    loop []

let multirange_to_string print xs =
  "{" ^ String.concat "," (List.map (to_string print) xs) ^ "}"
