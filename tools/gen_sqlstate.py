#!/usr/bin/env python3
"""Regenerate lib/sqlml/sqlstate.ml{,i} from PostgreSQL's errcodes.txt.

    curl -O https://raw.githubusercontent.com/postgres/postgres/master/src/backend/utils/errcodes.txt
    python3 tools/gen_sqlstate.py errcodes.txt
    dune build @fmt; dune promote   # @fmt exits nonzero on the diff; promote anyway

Constructors derive from the ERRCODE_ macro names, not the condition names:
PostgreSQL reuses five condition names across classes (string_data_right_truncation
is both 01004 and 22001) and the macros are unique.
"""
import re, sys, collections, pathlib

src = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else "errcodes.txt")
out = pathlib.Path(__file__).resolve().parent.parent / "lib" / "sqlml"

rows, classes, cur = [], [], None
for line in src.read_text().splitlines(True):
    m = re.match(r"^Section:\s*Class\s+(\S+)\s*-\s*(.+?)\s*$", line)
    if m:
        cur = (m.group(1), m.group(2)); classes.append(cur); continue
    f = line.split()
    if len(f) >= 4 and re.match(r"^[0-9A-Z]{5}$", f[0]):
        rows.append((f[0], f[2], f[3], cur[0]))

def ctor(s):
    s = re.sub(r"[^A-Za-z0-9]+", "_", s).strip("_").lower()
    if s and s[0].isdigit():
        s = "C" + s
    return s[0].upper() + s[1:]

ctors = [(c, ctor(m[len("ERRCODE_"):]) if m.startswith("ERRCODE_") else ctor(m), cond, cl)
         for c, m, cond, cl in rows]
assert len(set(x[1] for x in ctors)) == len(ctors), "constructor collision"
cls = [(code, ctor(desc), desc) for code, desc in classes]
assert len(set(x[1] for x in cls)) == len(cls)

mli = ['''(** PostgreSQL SQLSTATE codes.

    Generated from [src/backend/utils/errcodes.txt] in the PostgreSQL source by
    [tools/gen_sqlstate.py]; %d codes across %d classes. The point is to let a
    caller act on a failure rather than print it: retry a serialization
    failure, return 409 on a unique violation, 400 on a check violation.

    {[
      match Sqlml.Error.sqlstate e with
      | Some s when Sqlml.Sqlstate.is_retryable s -> retry ()
      | Some s when Sqlml.Sqlstate.is_unique_violation s -> conflict ()
      | _ -> internal_error ()
    ]} *)

type t
(** A five-character SQLSTATE. *)

val of_string : string -> t
val to_string : t -> string
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit

val name : t -> string
(** Canonical condition name, e.g. ["unique_violation"]. The code itself for
    anything PostgreSQL does not name. *)

(** {1 Conditions} *)

type condition =''' % (len(ctors), len(cls))]
for _, c, _, _ in ctors:
    mli.append("  | " + c)
mli.append("  | Other of string  (** a code this version of sqlml does not know *)")
mli.append('''
val condition : t -> condition

(** {1 Classes} *)

module Class : sig
  type t =''')
for _, c, _ in cls:
    mli.append("    | " + c)
mli.append('''    | Other of string

  val to_string : t -> string
end

val class_ : t -> Class.t

(** {1 Common predicates} *)

val is_unique_violation : t -> bool
val is_foreign_key_violation : t -> bool
val is_not_null_violation : t -> bool
val is_check_violation : t -> bool
val is_exclusion_violation : t -> bool

val is_integrity_violation : t -> bool
(** Any class 23 code. *)

val is_serialization_failure : t -> bool
(** [40001] or [40P01]: the transaction lost a race and should be retried. *)

val is_connection_failure : t -> bool
(** Any class 08 code. *)

val is_retryable : t -> bool
(** A serialization failure, a deadlock, or a connection problem: re-running the
    transaction may succeed. Anything else will fail the same way again. *)

val is_syntax_or_access_error : t -> bool
(** Class 42: a bug in the query or a missing object, not a runtime condition. *)

val all : t list
(** Every code in the table, in [errcodes.txt] order. %d codes.

    Codes outside the table still occur: extensions define their own, and
    PL/pgSQL [RAISE ... USING ERRCODE] accepts arbitrary five-character codes.
    Those classify as {!condition} [Other] while {!class_} still resolves from
    the first two characters. Client-side failures — a dropped socket, a
    malformed URI — carry no SQLSTATE at all and surface as [Error.Connect] or
    an [Execute] with [sqlstate = None]; the predicates here return [false] for
    them. *)''' % len(ctors))

ml = ["type t = string\n", "let of_string s = s", "let to_string s = s",
      "let equal = String.equal", "let pp fmt s = Format.pp_print_string fmt s\n",
      "type condition ="]
for _, c, _, _ in ctors:
    ml.append("  | " + c)
ml.append("  | Other of string\n")
ml.append("let condition = function")
for code, c, _, _ in ctors:
    ml.append('  | %-8s -> %s' % ('"%s"' % code, c))
ml.append("  | s -> Other s\n")
ml.append("let name = function")
for code, _, cond, _ in ctors:
    ml.append('  | %-8s -> %s' % ('"%s"' % code, '"%s"' % cond))
ml.append("  | s -> s\n")
ml.append("module Class = struct\n  type t =")
for _, c, _ in cls:
    ml.append("    | " + c)
ml.append("    | Other of string\n")
ml.append("  let to_string = function")
for _, c, desc in cls:
    ml.append('    | %-38s -> %s' % (c, '"%s"' % desc))
ml.append('    | Other s -> s\nend\n')
ml.append("let class_ s =\n  let c = if String.length s >= 2 then String.sub s 0 2 else s in\n  match c with")
for code, c, _ in cls:
    ml.append('  | %-6s -> Class.%s' % ('"%s"' % code, c))
ml.append("  | other -> Class.Other other\n")
ml.append('''let has_class c s = String.length s >= 2 && String.sub s 0 2 = c

let is_unique_violation s = s = "23505"
let is_foreign_key_violation s = s = "23503"
let is_not_null_violation s = s = "23502"
let is_check_violation s = s = "23514"
let is_exclusion_violation s = s = "23P01"
let is_integrity_violation s = has_class "23" s
let is_serialization_failure s = s = "40001" || s = "40P01"
let is_connection_failure s = has_class "08" s
let is_retryable s = is_serialization_failure s || is_connection_failure s
let is_syntax_or_access_error s = has_class "42" s
''')
ml.append("(* every code in the table, for tooling and for testing the table itself *)")
ml.append("let all = [ " + ";".join('"%s"' % c for c, _, _, _ in ctors) + " ]")

(out / "sqlstate.mli").write_text("\n".join(mli) + "\n")
(out / "sqlstate.ml").write_text("\n".join(ml) + "\n")
print("wrote", len(ctors), "codes,", len(cls), "classes")
