open Generated

let run ?(copy = false) ~check ~id conn =
  let bytes = "\000\255\\" in
  let items =
    Sqlml.Pg_array.make
      ~dimensions:[ { lower = 1; length = 3 } ]
      [ Some bytes; None; Some "" ]
  in
  let binary = Db.binary_containers_exn conn ~bytes ~items in
  check "bytea scalar and nested array"
    (binary.bytes = Some bytes && binary.items = Some items);
  let open Sqlml.Range in
  let span =
    Bounds { lower = Inclusive Int64.min_int; upper = Exclusive Int64.max_int }
  in
  let payload : Db.compiler_a_payload =
    {
      label = Some "comma, quote\" slash\\ (NULL)";
      states =
        Some
          (Sqlml.Pg_array.make
             ~dimensions:[ { lower = 0; length = 3 } ]
             [ Some (Db.Open : Db.compiler_a_status); None; Some Db.Closed ]);
      amount = Some Int64.max_int;
      span = Some span;
    }
  in
  let other : Db.compiler_b_payload = { label = Some ""; state = Some Db.Archived } in
  let empty : Db.compiler_a_payload =
    { label = None; states = None; amount = None; span = None }
  in
  let payloads =
    Sqlml.Pg_array.make
      ~dimensions:[ { lower = 1; length = 3 } ]
      [ Some payload; None; Some empty ]
  in
  let matrix =
    Sqlml.Pg_array.make
      ~dimensions:[ { lower = -2; length = 2 }; { lower = 4; length = 2 } ]
      [ Some Int64.min_int; None; Some Int64.max_int; Some 0L ]
  in
  let positives = Sqlml.Pg_array.of_list [ 1L; Int64.max_int ] in
  let spans = [ span ] in
  let words = Bounds { lower = Inclusive "a,\"\\"; upper = Exclusive "z)" } in
  ignore
    (Db.put_compiler_value_exn conn ~id ~payload ~other ~payloads ~matrix ~positives
       ~spans ~words ());
  let row = Db.get_compiler_value_exn conn ~id in
  check "named nested composite" (row.payload = Some payload);
  check "same-named composite in other schema" (row.other = Some other);
  check "composite array NULL versus all-NULL record" (row.payloads = Some payloads);
  check "multidimensional int8 with NULL and lower bounds" (row.matrix = Some matrix);
  check "domain over array of domains" (row.positives = Some positives);
  check "int8 multirange bounds" (row.spans = Some spans);
  check "custom range escaping" (row.words = Some words);
  let joined = Db.outer_join_compiler_exn conn in
  check "outer join null extension without override"
    (joined.id = None && joined.payload = None);
  ignore
    (Db.put_compiler_value_exn conn ~id ~payload:empty ~payloads ~matrix ~positives
       ~spans:[] ~words:Empty ());
  let row = Db.get_compiler_value_exn conn ~id in
  check "NULL composite differs from all-NULL composite"
    (row.payload = Some empty && row.other = None);
  check "empty ranges" (row.spans = Some [] && row.words = Some Empty);
  if copy then begin
    let outcome =
      Sqlml.transaction conn (fun tx ->
          let row : Db.Copy_compiler_value.params =
            {
              id = -id;
              payload = Some payload;
              other = Some other;
              payloads;
              matrix;
              positives;
              spans;
              words;
            }
          in
          ignore (Db.copy_compiler_value_exn tx [ row ]);
          let got = Db.get_compiler_value_exn tx ~id:(-id) in
          check "COPY nested containers"
            (got.payload = Some payload && got.matrix = Some matrix
           && got.words = Some words);
          Error (Sqlml.Error.Connect "rollback compiler COPY fixture"))
    in
    check "COPY fixture rolled back" (Result.is_error outcome)
  end;
  let bad = { payload with amount = Some (-1L) } in
  match
    Db.put_compiler_value conn ~id ~payload:bad ~payloads ~matrix ~positives ~spans ~words
      ()
  with
  | Error e ->
      check "domain constraint SQLSTATE"
        (Option.map Sqlml.Sqlstate.to_string (Sqlml.Error.sqlstate e) = Some "23514")
  | Ok _ -> check "domain constraint SQLSTATE" false
