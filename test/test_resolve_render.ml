(* Resolution errors (identifier guards, enum conflicts, row-type collisions)
   and a golden render: same inputs must produce byte-identical source. All
   offline, via Describe's explicit constructors. Regenerate the goldens with
   --update-golden after an intended output change. *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

open Sqlml_gen

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  go 0

let parse1 src =
  match Parse.of_string ~file:"t.sql" src with Ok [ q ] -> q | _ -> failwith "parse1"

let col ?(table = None) ?(table_oid = 0) ?(table_col = 0) ?(nullable = false)
    ?(labels = []) ?(elem = None) name type_name =
  Describe.v_column ~name ~type_name ~elem_type_name:elem ~table ~table_oid ~table_col
    ~nullable ~enum_labels:labels

let param ?(nullable = false) ?(labels = []) index pname ptype_name =
  Describe.v_param ~index ~pname ~ptype_name ~pelem_type_name:None ~penum_labels:labels
    ~pnullable:nullable

let described ?(model = None) src ~params ~columns =
  Describe.v_described ~query:(parse1 src) ~params ~columns ~model_table:model

let resolve d = Resolve.resolve Config.empty d

let () =
  (* ---------- identifier guards ---------- *)
  let bad_field =
    described "-- name: Q :one\nSELECT 1" ~params:[] ~columns:[ col "type" "int4" ]
  in
  (match resolve bad_field with
  | Error d -> check "keyword column rejected, named" (contains (Diag.to_string d) "type")
  | Ok _ -> check "keyword column rejected, named" false);

  let bad_param =
    described "-- name: Q :exec\nSELECT 1" ~params:[ param 1 "end" "int4" ] ~columns:[]
  in
  (match resolve bad_param with
  | Error d -> check "keyword param rejected" (contains (Diag.to_string d) "end")
  | Ok _ -> check "keyword param rejected" false);

  let ok_q =
    described "-- name: Q :one\nSELECT 1" ~params:[]
      ~columns:[ col "id" "int4"; col "email" "text" ]
  in
  check "plain query resolves" (Result.is_ok (resolve ok_q));

  (* ---------- enum guards ---------- *)
  let enum_q name labels =
    described "-- name: E :one\nSELECT 1" ~params:[] ~columns:[ col "s" name ~labels ]
  in
  let r1 = Result.get_ok (resolve (enum_q "status" [ "a"; "b" ])) in
  let r2 = Result.get_ok (resolve (enum_q "status" [ "a"; "c" ])) in
  (match Resolve.collect_enums [ r1; r2 ] with
  | Error d ->
      check "conflicting enum labels rejected" (contains (Diag.to_string d) "status")
  | Ok _ -> check "conflicting enum labels rejected" false);
  (match
     Resolve.collect_enums [ Result.get_ok (resolve (enum_q "user-status" [ "a" ])) ]
   with
  | Error _ -> check "invalid enum typname rejected" true
  | Ok _ -> check "invalid enum typname rejected" false);
  check "consistent enums collect"
    (Resolve.collect_enums [ r1; r1 ] = Ok [ ("status", [ "a"; "b" ]) ]);

  (* ---------- row-type collision names both origins ---------- *)
  let users_cols =
    [
      col "id" "int4" ~table:(Some "users") ~table_oid:7 ~table_col:1;
      col "email" "text" ~table:(Some "users") ~table_oid:7 ~table_col:2;
    ]
  in
  let shared =
    described "-- name: GetUserFull :one\nSELECT 1" ~params:[] ~columns:users_cols
      ~model:(Some "users")
  in
  let clashing =
    described "-- name: Users :many\nSELECT 1" ~params:[] ~columns:[ col "other" "int4" ]
  in
  let rs = [ Result.get_ok (resolve shared); Result.get_ok (resolve clashing) ] in
  (match Resolve.collect_rows rs with
  | Error d ->
      let m = Diag.to_string d in
      check "collision names both origins"
        (contains m "users_row" && contains m "query Users" && contains m "table users")
  | Ok _ -> check "collision names both origins" false);

  (* ---------- golden render ---------- *)
  let doc_q =
    described
      "-- name: GetThing :one\n\
       -- Docs with a comment terminator: *) and an opener (* inside.\n\
       SELECT 1"
      ~params:[ param 1 "id" "uuid" ]
      ~columns:[ col "id" "uuid"; col "note" "text" ~nullable:true ]
  in
  let dyn_q =
    described
      "-- name: FindThings :many\nSELECT a FROM t WHERE o = :o /*? AND e = :e */ LIMIT :n"
      ~params:[ param 1 "o" "uuid"; param 2 "e" "text"; param 3 "n" "int8" ]
      ~columns:[ col "a" "text" ]
  in
  (match Emit.generate ~src:"golden" [ doc_q; dyn_q ] with
  | Error d -> check ("golden renders: " ^ Diag.to_string d) false
  | Ok (mli, ml) ->
      let compare_golden suffix actual =
        let path = "render_golden." ^ suffix ^ ".expected" in
        if Array.mem "--update-golden" Sys.argv then begin
          Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc actual);
          check ("golden " ^ suffix ^ " written") true
        end
        else begin
          let expected = In_channel.with_open_bin path In_channel.input_all in
          if expected = actual then check ("golden " ^ suffix) true
          else begin
            (match Run.first_difference ~path ~expected ~actual with
            | Some (Run.Differs { line; expected; actual; _ }) ->
                Printf.printf "  %s:%d\n  expected: %s\n  actual:   %s\n" path line
                  expected actual
            | _ -> ());
            check ("golden " ^ suffix) false
          end
        end
      in
      compare_golden "mli" mli;
      compare_golden "ml" ml;
      check "doc comment still closes exactly once" (not (contains ml "terminator: *)")));

  (* ---------- :copy resolves to a COPY statement, or fails naming the shape ---------- *)
  let copy_q sql =
    described sql
      ~params:[ param 1 "a" "uuid"; param 2 "b" "text" ~nullable:true ]
      ~columns:[]
  in
  (match
     resolve (copy_q "-- name: BulkAdd :copy\nINSERT INTO t (a, b) VALUES (:a, :b)")
   with
  | Error d -> check ("copy resolves: " ^ Diag.to_string d) false
  | Ok r -> (
      check "copy target extracted" (r.Resolve.copy = Some ("t", [ "a"; "b" ]));
      match
        Emit.generate ~src:"s"
          [ copy_q "-- name: BulkAdd :copy\nINSERT INTO t (a, b) VALUES (:a, :b)" ]
      with
      | Error d -> check ("copy renders: " ^ Diag.to_string d) false
      | Ok (mli, ml) ->
          check "copy_sql is built from the verified columns"
            (contains ml "COPY t (a, b) FROM STDIN");
          check "copy wrapper takes a row list" (contains mli "params list");
          check "copy module satisfies COPY" (contains mli "Sqlml.Query.COPY")));
  (match resolve (copy_q "-- name: BulkAdd :copy\nUPDATE t SET a = :a WHERE b = :b") with
  | Error d -> check "non-INSERT copy rejected" (contains (Diag.to_string d) "INSERT")
  | Ok _ -> check "non-INSERT copy rejected" false);
  (match
     resolve
       (described "-- name: BulkAdd :copy\nINSERT INTO t (a) VALUES (:a) RETURNING a"
          ~params:[ param 1 "a" "uuid" ]
          ~columns:[ col "a" "uuid" ])
   with
  | Error d -> check "RETURNING rejected" (contains (Diag.to_string d) "RETURNING")
  | Ok _ -> check "RETURNING rejected" false);
  (match
     resolve (copy_q "-- name: BulkAdd :copy\nINSERT INTO t (a, b, c) VALUES (:a, :b)")
   with
  | Error _ -> check "column/parameter mismatch rejected" true
  | Ok _ -> check "column/parameter mismatch rejected" false);

  print_endline "all good"
