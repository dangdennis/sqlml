type dimension = { lower : int; length : int }
type 'a t = { dimensions : dimension list; elements : 'a option list }

let make ~dimensions elements =
  if List.length dimensions > 6 then invalid_arg "array: more than six dimensions";
  let size =
    List.fold_left
      (fun n d ->
        if d.length <= 0 || n > max_int / d.length || d.lower > max_int - d.length + 1
        then invalid_arg "array: invalid dimensions";
        n * d.length)
      (if dimensions = [] then 0 else 1)
      dimensions
  in
  if size <> List.length elements then
    invalid_arg "array: element count disagrees with dimensions";
  { dimensions; elements }

let dimensions a = a.dimensions
let elements a = a.elements

let of_list xs =
  make
    ~dimensions:(if xs = [] then [] else [ { lower = 1; length = List.length xs } ])
    (List.map Option.some xs)

let to_list a =
  (match a.dimensions with
  | [] | [ { lower = 1; _ } ] -> ()
  | _ -> invalid_arg "array: not a list with lower bound one");
  List.map (function Some x -> x | None -> invalid_arg "array: NULL element") a.elements

let of_string ?(delimiter = ',') parse input =
  let s = String.trim input in
  let n = String.length s and p = ref 0 in
  let expect c =
    if !p >= n || s.[!p] <> c then invalid_arg "array: unexpected syntax" else incr p
  in
  let integer stop =
    let start = !p in
    while !p < n && s.[!p] <> stop do
      incr p
    done;
    if !p = n then invalid_arg "array: missing dimension delimiter";
    int_of_string (String.sub s start (!p - start))
  in
  let rec bounds acc =
    if !p < n && s.[!p] = '[' then begin
      incr p;
      let lower = integer ':' in
      expect ':';
      let upper = integer ']' in
      expect ']';
      if
        upper < lower
        || Int64.sub (Int64.of_int upper) (Int64.of_int lower) >= Int64.of_int max_int
      then invalid_arg "array: invalid bounds";
      bounds ({ lower; length = upper - lower + 1 } :: acc)
    end
    else List.rev acc
  in
  let explicit = bounds [] in
  if explicit <> [] then expect '=';
  let rec array depth =
    if depth > 6 then invalid_arg "array: more than six dimensions";
    expect '{';
    if !p < n && s.[!p] = '}' then (
      incr p;
      ([], []))
    else begin
      let shape = ref None and values = ref [] and count = ref 0 in
      let rec item () =
        let dims, xs =
          if !p < n && s.[!p] = '{' then array (depth + 1)
          else begin
            let raw, quoted =
              Pg_text.token s p ~stop:(fun c -> c = delimiter || c = '}')
            in
            if (not quoted) && String.trim raw = "" then
              invalid_arg "array: missing element";
            let value =
              if (not quoted) && String.lowercase_ascii (String.trim raw) = "null" then
                None
              else
                Some
                  (Pg_text.context (Printf.sprintf "array element %d" !count) (fun () ->
                       parse (if quoted then raw else String.trim raw)))
            in
            ([], [ value ])
          end
        in
        (match !shape with
        | None -> shape := Some dims
        | Some d when d = dims -> ()
        | _ -> invalid_arg "array: ragged dimensions");
        incr count;
        values := List.rev_append xs !values;
        if !p < n && s.[!p] = delimiter then (
          incr p;
          item ())
        else expect '}'
      in
      item ();
      (!count :: Option.get !shape, List.rev !values)
    end
  in
  let shape, xs = array 1 in
  if !p <> n then invalid_arg "array: trailing text";
  let dimensions =
    if explicit = [] then List.map (fun length -> { lower = 1; length }) shape
    else begin
      if List.map (fun d -> d.length) explicit <> shape then
        invalid_arg "array: bounds disagree with contents";
      explicit
    end
  in
  make ~dimensions xs

let to_string ?(delimiter = ',') print a =
  let rest = ref a.elements in
  let rec body = function
    | [] -> (
        match !rest with
        | x :: xs -> (
            rest := xs;
            match x with None -> "NULL" | Some x -> Pg_text.quote (print x))
        | [] -> invalid_arg "array: missing value")
    | d :: ds ->
        "{"
        ^ String.concat (String.make 1 delimiter) (List.init d.length (fun _ -> body ds))
        ^ "}"
  in
  if a.dimensions = [] then "{}"
  else
    let prefix =
      if List.for_all (fun d -> d.lower = 1) a.dimensions then ""
      else
        String.concat ""
          (List.map
             (fun d -> Printf.sprintf "[%d:%d]" d.lower (d.lower + d.length - 1))
             a.dimensions)
        ^ "="
    in
    prefix ^ body a.dimensions
