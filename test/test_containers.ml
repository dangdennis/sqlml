let check label condition = if not condition then failwith label

let rejects label f =
  match f () with
  | _ -> failwith (label ^ ": accepted malformed input")
  | (exception Invalid_argument _) | (exception Failure _) -> ()

let () =
  let module A = Sqlml.Pg_array in
  let module R = Sqlml.Range in
  let tricky = [ ""; "NULL"; "a,b"; "a\\b"; "a\"b"; "(x)"; "{x}"; "\n\t" ] in
  List.iteri
    (fun i s ->
      let a =
        A.make
          ~dimensions:[ { lower = i - 4; length = 2 }; { lower = 2; length = 2 } ]
          [ Some s; None; Some "NULL"; Some "" ]
      in
      check "array round trip" (A.of_string Fun.id (A.to_string Fun.id a) = a))
    tricky;
  check "empty array" (A.dimensions (A.of_string Fun.id "{}") = []);
  check "escaped NULL is text"
    (A.elements (A.of_string Fun.id "{N\\ULL}") = [ Some "NULL" ]);
  check "custom delimiter"
    (A.to_list (A.of_string ~delimiter:';' Fun.id "{\"x;y\";z}") = [ "x;y"; "z" ]);
  List.iter
    (fun text -> rejects text (fun () -> A.of_string Fun.id text))
    [ "{"; "{\"x}"; "{a,}"; "{{a},{b,c}}"; "[0:3]={a}"; "{a}junk"; "{\"a\"b}" ];
  rejects "dimension mismatch" (fun () ->
      A.make ~dimensions:[ { lower = 1; length = 2 } ] [ Some 1 ]);
  rejects "NULL list conversion" (fun () -> A.to_list (A.of_string Fun.id "{NULL}"));
  rejects "bounds list conversion" (fun () -> A.to_list (A.of_string Fun.id "[0:0]={a}"));
  List.iter
    (fun s ->
      let fields = [| Some s; None; Some "" |] in
      check "composite escaping"
        (Sqlml.Composite.of_string (Sqlml.Composite.to_string fields) = fields))
    tricky;
  List.iter
    (fun s -> rejects s (fun () -> Sqlml.Composite.of_string s))
    [ "(\"x)"; "(a)junk"; "(a))"; "(\"x\"y)" ];
  let bounds = [ R.Unbounded; R.Inclusive ""; R.Exclusive "a,\"\\)" ] in
  List.iter
    (fun lower ->
      List.iter
        (fun upper ->
          let r = R.Bounds { lower; upper } in
          check "range round trip" (R.of_string Fun.id (R.to_string Fun.id r) = r))
        bounds)
    bounds;
  check "empty range" (R.of_string Fun.id "empty" = R.Empty);
  let rs = [ R.Empty; R.Bounds { lower = R.Inclusive "x"; upper = R.Unbounded } ] in
  check "multirange round trip"
    (R.multirange_of_string Fun.id (R.multirange_to_string Fun.id rs) = rs);
  List.iter
    (fun s -> rejects s (fun () -> R.of_string Fun.id s))
    [ "[a,b"; "[a,b]x"; "[\"a,b)"; "emptyx" ];
  List.iter
    (fun n -> check "int64 codec" (Sqlml.Row.int64 [| Sqlml.Value.of_int64 n |] 0 = n))
    [ Int64.min_int; Int64.max_int ];
  print_endline "container codecs: all good"
