(* The Sqlstate table is generated from PostgreSQL's errcodes.txt; this checks
   the generated table is self-consistent, and that codes from outside it
   degrade the way the interface promises. *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

open Sqlml

let () =
  check "table has all 262 codes" (List.length Sqlstate.all = 262);

  (* every code in the table classifies: a named condition, a named class, and
     a condition name distinct from the raw code *)
  check "every code has a condition"
    (List.for_all
       (fun s -> match Sqlstate.condition s with Sqlstate.Other _ -> false | _ -> true)
       Sqlstate.all);
  check "every code has a class"
    (List.for_all
       (fun s ->
         match Sqlstate.class_ s with Sqlstate.Class.Other _ -> false | _ -> true)
       Sqlstate.all);
  check "every code has a condition name"
    (List.for_all (fun s -> Sqlstate.name s <> Sqlstate.to_string s) Sqlstate.all);
  check "codes are unique"
    (List.length (List.sort_uniq compare (List.map Sqlstate.to_string Sqlstate.all)) = 262);

  (* the specific codes the predicates promise *)
  let s c = Sqlstate.of_string c in
  check "23505 unique" (Sqlstate.is_unique_violation (s "23505"));
  check "23503 fk" (Sqlstate.is_foreign_key_violation (s "23503"));
  check "23502 not-null" (Sqlstate.is_not_null_violation (s "23502"));
  check "23514 check" (Sqlstate.is_check_violation (s "23514"));
  check "23P01 exclusion" (Sqlstate.is_exclusion_violation (s "23P01"));
  check "class 23 is integrity"
    (List.for_all Sqlstate.is_integrity_violation
       (List.filter
          (fun c ->
            String.length (Sqlstate.to_string c) = 5
            && String.sub (Sqlstate.to_string c) 0 2 = "23")
          Sqlstate.all));
  check "40001 retryable" (Sqlstate.is_retryable (s "40001"));
  check "40P01 retryable" (Sqlstate.is_retryable (s "40P01"));
  check "40002 not retryable" (not (Sqlstate.is_retryable (s "40002")));
  check "08006 connection + retryable"
    (Sqlstate.is_connection_failure (s "08006") && Sqlstate.is_retryable (s "08006"));
  check "42703 syntax/access" (Sqlstate.is_syntax_or_access_error (s "42703"));
  check "23505 not retryable" (not (Sqlstate.is_retryable (s "23505")));

  (* a code outside the table: extensions and RAISE ... USING ERRCODE *)
  let custom = s "ZX123" in
  check "unknown code -> Other" (Sqlstate.condition custom = Sqlstate.Other "ZX123");
  check "unknown code keeps its name" (Sqlstate.name custom = "ZX123");
  check "unknown code, unknown class" (Sqlstate.class_ custom = Sqlstate.Class.Other "ZX");
  (* an unknown code in a KNOWN class still classifies by class *)
  let custom_23 = s "23999" in
  check "unknown code in class 23 still integrity"
    (Sqlstate.is_integrity_violation custom_23);
  check "PL/pgSQL P0001 present"
    (Sqlstate.condition (s "P0001") = Sqlstate.Raise_exception);

  print_endline "all good"
