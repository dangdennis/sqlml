/* Minimal libpq binding, just enough to describe a prepared statement.
 *
 * Why this exists rather than reusing postgresql-ocaml: that library does not
 * expose PQftable / PQftablecol, and those two fields are the whole point.
 * They are what tells us which table.column a result column came from, which
 * is what drives nullability (pg_attribute.attnotnull) and shared model type
 * detection. Everything else here is scaffolding to reach them.
 *
 * Going through libpq rather than the wire protocol means authentication --
 * SCRAM-SHA-256, TLS, .pgpass, everything -- is libpq's problem, not ours.
 *
 * Handles are passed as nativeint rather than custom blocks. Lifetimes are
 * managed explicitly by the caller; this is used by a short-lived generator
 * process, not a long-running service. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <libpq-fe.h>
#include <stdlib.h>
#include <string.h>
#include <caml/threads.h>
#include <caml/fail.h>

#define Conn_val(v) ((PGconn *)Nativeint_val(v))
#define Res_val(v) ((PGresult *)Nativeint_val(v))

/* ---------- connection ---------- */

/* Blocking libpq calls run with the OCaml runtime lock released, so a slow
 * server does not stall every other domain. Anything read from the OCaml heap
 * must be copied to C memory first: String_val pointers are invalid once the
 * lock is released and the GC may move the blocks. */
CAMLprim value sqlml_pq_connect(value conninfo)
{
  CAMLparam1(conninfo);
  char *ci = caml_stat_strdup(String_val(conninfo));
  caml_release_runtime_system();
  PGconn *c = PQconnectdb(ci);
  caml_acquire_runtime_system();
  caml_stat_free(ci);
  CAMLreturn(caml_copy_nativeint((intnat)c));
}

CAMLprim value sqlml_pq_connect_ok(value conn)
{
  CAMLparam1(conn);
  PGconn *c = Conn_val(conn);
  CAMLreturn(Val_bool(c != NULL && PQstatus(c) == CONNECTION_OK));
}

CAMLprim value sqlml_pq_error_message(value conn)
{
  CAMLparam1(conn);
  PGconn *c = Conn_val(conn);
  const char *m = (c == NULL) ? "null connection" : PQerrorMessage(c);
  CAMLreturn(caml_copy_string(m == NULL ? "" : m));
}

CAMLprim value sqlml_pq_finish(value conn)
{
  CAMLparam1(conn);
  PGconn *c = Conn_val(conn);
  if (c != NULL) PQfinish(c);
  CAMLreturn(Val_unit);
}

/* ---------- statements ---------- */

/* nParams = 0 lets the server infer parameter types, which is exactly what we
 * want -- the inferred types are the answer we are after. */
CAMLprim value sqlml_pq_prepare(value conn, value name, value sql)
{
  CAMLparam3(conn, name, sql);
  PGconn *c = Conn_val(conn);
  char *nm = caml_stat_strdup(String_val(name));
  char *q = caml_stat_strdup(String_val(sql));
  caml_release_runtime_system();
  PGresult *r = PQprepare(c, nm, q, 0, NULL);
  caml_acquire_runtime_system();
  caml_stat_free(nm);
  caml_stat_free(q);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

CAMLprim value sqlml_pq_describe_prepared(value conn, value name)
{
  CAMLparam2(conn, name);
  PGconn *c = Conn_val(conn);
  char *nm = caml_stat_strdup(String_val(name));
  caml_release_runtime_system();
  PGresult *r = PQdescribePrepared(c, nm);
  caml_acquire_runtime_system();
  caml_stat_free(nm);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

CAMLprim value sqlml_pq_exec(value conn, value sql)
{
  CAMLparam2(conn, sql);
  PGconn *c = Conn_val(conn);
  char *q = caml_stat_strdup(String_val(sql));
  caml_release_runtime_system();
  PGresult *r = PQexec(c, q);
  caml_acquire_runtime_system();
  caml_stat_free(q);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

/* ---------- results ---------- */

/* Maps to ExecStatusType; 1 = COMMAND_OK, 2 = TUPLES_OK. */
CAMLprim value sqlml_pq_result_status(value res)
{
  CAMLparam1(res);
  PGresult *r = Res_val(res);
  CAMLreturn(Val_int(r == NULL ? -1 : (int)PQresultStatus(r)));
}

CAMLprim value sqlml_pq_result_error(value res)
{
  CAMLparam1(res);
  PGresult *r = Res_val(res);
  const char *m = (r == NULL) ? "null result" : PQresultErrorMessage(r);
  CAMLreturn(caml_copy_string(m == NULL ? "" : m));
}

CAMLprim value sqlml_pq_clear(value res)
{
  CAMLparam1(res);
  PGresult *r = Res_val(res);
  if (r != NULL) PQclear(r);
  CAMLreturn(Val_unit);
}

CAMLprim value sqlml_pq_nparams(value res)
{
  CAMLparam1(res);
  CAMLreturn(Val_int(PQnparams(Res_val(res))));
}

CAMLprim value sqlml_pq_paramtype(value res, value i)
{
  CAMLparam2(res, i);
  CAMLreturn(Val_long((long)PQparamtype(Res_val(res), Int_val(i))));
}

CAMLprim value sqlml_pq_nfields(value res)
{
  CAMLparam1(res);
  CAMLreturn(Val_int(PQnfields(Res_val(res))));
}

CAMLprim value sqlml_pq_fname(value res, value i)
{
  CAMLparam2(res, i);
  const char *n = PQfname(Res_val(res), Int_val(i));
  CAMLreturn(caml_copy_string(n == NULL ? "" : n));
}

CAMLprim value sqlml_pq_ftype(value res, value i)
{
  CAMLparam2(res, i);
  CAMLreturn(Val_long((long)PQftype(Res_val(res), Int_val(i))));
}

/* The two accessors this whole file exists for. PQftable returns InvalidOid (0)
 * when the column is not a simple reference to a table column -- an expression,
 * a literal, an aggregate. PQftablecol likewise returns 0. */
CAMLprim value sqlml_pq_ftable(value res, value i)
{
  CAMLparam2(res, i);
  CAMLreturn(Val_long((long)PQftable(Res_val(res), Int_val(i))));
}

CAMLprim value sqlml_pq_ftablecol(value res, value i)
{
  CAMLparam2(res, i);
  CAMLreturn(Val_int(PQftablecol(Res_val(res), Int_val(i))));
}

/* ---------- tuples (for catalog queries) ---------- */

CAMLprim value sqlml_pq_ntuples(value res)
{
  CAMLparam1(res);
  CAMLreturn(Val_int(PQntuples(Res_val(res))));
}

CAMLprim value sqlml_pq_getvalue(value res, value row, value col)
{
  CAMLparam3(res, row, col);
  const char *v = PQgetvalue(Res_val(res), Int_val(row), Int_val(col));
  CAMLreturn(caml_copy_string(v == NULL ? "" : v));
}

CAMLprim value sqlml_pq_getisnull(value res, value row, value col)
{
  CAMLparam3(res, row, col);
  CAMLreturn(Val_bool(PQgetisnull(Res_val(res), Int_val(row), Int_val(col))));
}

/* ---------- executing ---------- */

/* Copy a string-option array of parameters out of the OCaml heap, so the
 * runtime lock can be released while libpq blocks. A parameter containing NUL
 * is rejected (returns -1): libpq would silently truncate it at the NUL,
 * sending a different value than the caller supplied. */
static int copy_params(value params, int n, char ***out)
{
  char **vals = NULL;
  if (n > 0) vals = (char **)caml_stat_alloc((size_t)n * sizeof(char *));
  for (int i = 0; i < n; i++) {
    value p = Field(params, i);
    if (Is_block(p)) {
      value sv = Field(p, 0);
      size_t len = caml_string_length(sv);
      if (memchr(String_val(sv), 0, len) != NULL) {
        for (int j = 0; j < i; j++)
          if (vals[j]) caml_stat_free(vals[j]);
        if (vals) caml_stat_free(vals);
        return -1;
      }
      vals[i] = caml_stat_strdup(String_val(sv));
    }
    else vals[i] = NULL;
  }
  *out = vals;
  return 0;
}

static void free_params(char **vals, int n)
{
  for (int i = 0; i < n; i++)
    if (vals && vals[i]) caml_stat_free(vals[i]);
  if (vals) caml_stat_free(vals);
}

/* [params] is a string option array; None becomes SQL NULL.
 *
 * paramTypes is NULL on purpose, so the server infers each parameter's type
 * from context. Forcing them to text -- which is what a statically-typed
 * client layer naturally does -- breaks comparisons like `WHERE uuid_col = $1`
 * with "operator does not exist: uuid = text". */
CAMLprim value sqlml_pq_exec_params(value conn, value sql, value params)
{
  CAMLparam3(conn, sql, params);
  PGconn *c = Conn_val(conn);
  int n = (int)Wosize_val(params);
  char **vals = NULL;
  if (copy_params(params, n, &vals) != 0)
    caml_invalid_argument("sqlml: parameter contains a NUL byte");
  char *q = caml_stat_strdup(String_val(sql));
  caml_release_runtime_system();
  PGresult *r = PQexecParams(c, q, n, NULL, (const char *const *)vals, NULL, NULL, 0);
  caml_acquire_runtime_system();
  caml_stat_free(q);
  free_params(vals, n);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

/* Rows affected by an INSERT/UPDATE/DELETE, as reported by the command tag. */
CAMLprim value sqlml_pq_cmd_tuples(value res)
{
  CAMLparam1(res);
  const char *s = PQcmdTuples(Res_val(res));
  CAMLreturn(Val_long((s == NULL || *s == '\0') ? 0 : strtoll(s, NULL, 10)));
}

/* PQresultErrorField, for the diagnostic fields that let a caller act on a
 * failure rather than just print it. PG_DIAG_SQLSTATE ('C') is the important
 * one; CONSTRAINT_NAME ('n') turns "something was taken" into "email was
 * taken". Returns "" when the field is absent. */
CAMLprim value sqlml_pq_result_error_field(value res, value field)
{
  CAMLparam2(res, field);
  PGresult *r = Res_val(res);
  const char *v = (r == NULL) ? NULL : PQresultErrorField(r, Int_val(field));
  CAMLreturn(caml_copy_string(v == NULL ? "" : v));
}

/* Execute a previously prepared statement. Same parameter convention as
 * sqlml_pq_exec_params: text format, None becomes NULL. */
CAMLprim value sqlml_pq_exec_prepared(value conn, value name, value params)
{
  CAMLparam3(conn, name, params);
  PGconn *c = Conn_val(conn);
  int n = (int)Wosize_val(params);
  char **vals = NULL;
  if (copy_params(params, n, &vals) != 0)
    caml_invalid_argument("sqlml: parameter contains a NUL byte");
  char *nm = caml_stat_strdup(String_val(name));
  caml_release_runtime_system();
  PGresult *r = PQexecPrepared(c, nm, n, (const char *const *)vals, NULL, NULL, 0);
  caml_acquire_runtime_system();
  caml_stat_free(nm);
  free_params(vals, n);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}
