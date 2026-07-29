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

#define Conn_val(v) ((PGconn *)Nativeint_val(v))
#define Res_val(v) ((PGresult *)Nativeint_val(v))

/* ---------- connection ---------- */

CAMLprim value sqlml_pq_connect(value conninfo)
{
  CAMLparam1(conninfo);
  PGconn *c = PQconnectdb(String_val(conninfo));
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
  PGresult *r = PQprepare(Conn_val(conn), String_val(name), String_val(sql), 0, NULL);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

CAMLprim value sqlml_pq_describe_prepared(value conn, value name)
{
  CAMLparam2(conn, name);
  PGresult *r = PQdescribePrepared(Conn_val(conn), String_val(name));
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

CAMLprim value sqlml_pq_exec(value conn, value sql)
{
  CAMLparam2(conn, sql);
  PGresult *r = PQexec(Conn_val(conn), String_val(sql));
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

CAMLprim value sqlml_pq_fmod(value res, value i)
{
  CAMLparam2(res, i);
  CAMLreturn(Val_int(PQfmod(Res_val(res), Int_val(i))));
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

/* [params] is a string option array; None becomes SQL NULL.
 *
 * paramTypes is NULL on purpose, so the server infers each parameter's type
 * from context. Forcing them to text -- which is what a statically-typed
 * client layer naturally does -- breaks comparisons like `WHERE uuid_col = $1`
 * with "operator does not exist: uuid = text". */
CAMLprim value sqlml_pq_exec_params(value conn, value sql, value params)
{
  CAMLparam3(conn, sql, params);
  int n = (int)Wosize_val(params);
  const char **vals = NULL;
  if (n > 0) vals = (const char **)caml_stat_alloc((size_t)n * sizeof(char *));
  for (int i = 0; i < n; i++) {
    value p = Field(params, i);
    vals[i] = Is_block(p) ? String_val(Field(p, 0)) : NULL;
  }
  PGresult *r =
      PQexecParams(Conn_val(conn), String_val(sql), n, NULL, vals, NULL, NULL, 0);
  if (vals) caml_stat_free((void *)vals);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

/* Rows affected by an INSERT/UPDATE/DELETE, as reported by the command tag. */
CAMLprim value sqlml_pq_cmd_tuples(value res)
{
  CAMLparam1(res);
  const char *s = PQcmdTuples(Res_val(res));
  CAMLreturn(Val_int((s == NULL || *s == '\0') ? 0 : atoi(s)));
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
