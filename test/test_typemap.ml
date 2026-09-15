(* The Postgres-name -> OCaml-type table and the decoder/encoder expressions
   the renderer splices. These strings ARE the generated code; a typo here is a
   compile error in every user's project. *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

open Sqlml_gen

let t ?custom ?elem ?(labels = []) ?(nullable = false) name =
  let pg = Pg_type.legacy ~name ~element:elem ~labels in
  Typemap.of_type ~custom:(fun _ -> custom) pg ~nullable

let () =
  check "int8 -> int64" (Option.map Typemap.ocaml_type (t "int8") = Some "int64");
  check "numeric -> Decimal.t"
    (Option.map Typemap.ocaml_type (t "numeric") = Some "Decimal.t");
  check "timestamptz -> Ptime.t"
    (Option.map Typemap.ocaml_type (t "timestamptz") = Some "Ptime.t");
  check "jsonb -> Yojson"
    (Option.map Typemap.ocaml_type (t "jsonb") = Some "Yojson.Safe.t");
  check "date -> Ptime.date" (Option.map Typemap.ocaml_type (t "date") = Some "Ptime.date");
  check "interval -> Sqlml.Interval.t"
    (Option.map Typemap.ocaml_type (t "interval") = Some "Sqlml.Interval.t");
  check "timetz stays string" (Option.map Typemap.ocaml_type (t "timetz") = Some "string");

  (* the hard-error invariant: unmapped is None, never a silent string *)
  check "unmapped type is None" (t "hstore" = None);
  check "unmapped array element is None" (t "_hstore" ~elem:"hstore" = None);

  (* nullability wraps, arrays wrap, and they compose *)
  check "nullable wraps option"
    (Option.map Typemap.ocaml_type (t "text" ~nullable:true) = Some "string option");
  check "array of uuid"
    (Option.map Typemap.ocaml_type (t "_uuid" ~elem:"uuid")
    = Some "(Uuidm.t) Sqlml.Pg_array.t");
  check "nullable array"
    (Option.map Typemap.ocaml_type (t "_int4" ~elem:"int4" ~nullable:true)
    = Some "(int) Sqlml.Pg_array.t option");
  check "array of enum decodes via _of_string"
    (Option.map Typemap.decoder (t "_status" ~elem:"status" ~labels:[ "a"; "b" ])
    = Some
        "(Sqlml.Row.custom (Sqlml.Pg_array.of_string ~delimiter:',' \
         public_status_of_string))");

  (* enums *)
  check "enum ocaml type is the typname"
    (Option.map Typemap.ocaml_type (t "user_status" ~labels:[ "active" ])
    = Some "public_user_status");
  check "enum decoder"
    (Option.map Typemap.decoder (t "user_status" ~labels:[ "a" ])
    = Some "public_user_status_of_row");

  (* customs replace whatever the type would otherwise be, including enums *)
  let c =
    {
      Typemap.c_ocaml = "Email.t";
      c_of_string = "Email.of_string";
      c_to_string = "Email.to_string";
    }
  in
  check "custom type wins"
    (Option.map Typemap.ocaml_type (t "citext" ~custom:c) = Some "Email.t");
  check "custom beats enum"
    (Option.map Typemap.ocaml_type (t "user_status" ~labels:[ "a" ] ~custom:c)
    = Some "Email.t");
  check "custom decoder splices of_string"
    (Option.map Typemap.decoder (t "citext" ~custom:c)
    = Some "(Sqlml.Row.custom Email.of_string)");
  check "custom encoder goes through to_string"
    (Option.map Typemap.encoder (t "citext" ~custom:c)
    = Some "(fun x -> Sqlml.Value.of_string (Email.to_string x))");

  (* constructor naming *)
  check "label to constructor" (Typemap.constructor_of_label "in-progress" = "In_progress");
  check "digit-leading label" (Typemap.constructor_of_label "2fa" = "N2fa");

  print_endline "all good"
