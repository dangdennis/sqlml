# Roadmap

Ordered by what a real application needs first. Nothing here is committed to;
it is a plan to argue with.

## 1. SQLSTATE in errors — done

Today `Error.Execute` carries a message string, so a caller cannot tell these
apart:

| SQLSTATE | Meaning | Right response |
| --- | --- | --- |
| `23505` | unique_violation | 409, do not retry |
| `23503` | foreign_key_violation | 400 |
| `23514` | check_violation | 400 |
| `40001` | serialization_failure | retry the transaction |
| `40P01` | deadlock_detected | retry the transaction |
| `57014` | query_canceled | timeout |
| `08006` | connection_failure | reconnect, retry |

A web application has to make these distinctions, and matching on the message
text is fragile and locale-dependent.

Proposed:

```ocaml
type sqlstate = private string

type t =
  | Execute of
      { query : string
      ; sql : string
      ; sqlstate : sqlstate option   (* None for client-side failures *)
      ; message : string
      ; detail : string option
      ; constraint_name : string option
      }
  | ...

val is_unique_violation : t -> bool
val is_retryable : t -> bool          (* 40001, 40P01, and connection loss *)
val constraint_violated : t -> string option
```

`constraint_name` is what turns "something was already taken" into "email was
already taken" without parsing prose.

Shipped. `Sqlml.Sqlstate` covers all 262 codes across 43 classes, generated from
`errcodes.txt` in the PostgreSQL source. libpq supplies every diagnostic field;
Caqti supplies the code and message only, which is all it exposes.

## 2. Naming, and overrides — done

Generated names come from three places, and they share one namespace:

- shared model row types, from a table name — `users` becomes `users_row`
- per-query row types, from the query name — `GetUser` becomes `get_user_row`
- enums, from the Postgres type name — `user_status`

A query named `Users` therefore collides with the model for table `users`. That
is now detected and reported with both origins, but detection is not a fix.

How the neighbours handle it:

- **sqlc** singularizes table names (`authors` becomes `Author`), names query
  results `<Query>Row`, and provides a `rename:` config. It has the same
  collision class and documents the same limitation: one namespace, so a table
  and a column cannot rename independently.
- **Squirrel** generates one row type per query and no shared models at all.
  No sharing, so no collisions — and no way to write a function that works
  across two queries returning the same table.

We chose sharing deliberately, so we inherit sqlc's problem and take its
escape hatch: a `sqlml.toml` beside the queries directory.

```toml
[rename]
users = "user"              # users_row -> user_row
"users.display_name" = "name"
```

(Custom OCaml types for columns are separate — see 3.)

Deliberately not singularizing automatically. English pluralization is a swamp
(`data`, `series`, `status`, `people`), and sqlc users hit it constantly. An
explicit rename is longer and always right.

## 3. Custom type mapping — done

The largest single lever on how the generated API feels. Today a `uuid` column
is `Uuidm.t` and an email is `string`; there is no way to say a column is a
`User_id.t` or an `Email.t`.

```toml
[types."users.id"]
ocaml = "User_id.t"
of_string = "User_id.of_string"
to_string = "User_id.to_string"

[types.uuid]
ocaml = "Id.t"
of_string = "Id.of_string"
to_string = "Id.to_string"
```

Per-column overrides win over per-type. The generator splices the named
functions into the decoder and encoder it already emits, so this needs no
runtime support — `Typemap.t` grows a `Custom` case and the machinery is
unchanged. sqlc's most requested feature.

## 4. Transactions: isolation and savepoints — done

`transaction` currently issues a bare `BEGIN`. Two gaps:

- No isolation level. `transaction ~isolation:`Serializable` is required for
  anything doing read-modify-write, and serializable is unusable without a
  retry loop, which is unusable without (1).
- Nested transactions are undefined. A `transaction` inside a `transaction`
  issues a second `BEGIN`, which PostgreSQL warns about and ignores, so the
  inner rollback silently does nothing. Savepoints fix this properly.

```ocaml
val transaction :
  ?isolation:[ `Read_committed | `Repeatable_read | `Serializable ] ->
  ?retry:int ->
  conn -> (conn -> ('a, Error.t) result) -> ('a, Error.t) result
```

With `?retry`, a `40001` or `40P01` re-runs the body — the standard serializable
pattern, and the reason (1) comes first.

## 5. Streaming, and a stricter :one — done

Two small, independent wins.

`fetch_all` builds a list, which is the wrong shape for an export of a million
rows. Caqti already streams; libpq needs a cursor or single-row mode.

```ocaml
val fetch_stream : (module Q : Query.MANY) -> conn -> Q.params -> (Q.row -> unit) -> (unit, Error.t) result
```

And a `:one!` cardinality returning the row directly, erroring when absent,
since a good half of `:one` call sites immediately unwrap the option.

## 6. Dynamic filters — done (optional blocks)

The common request the SQL-first model has no clean answer to: "filter by name
if provided, and by status if provided."

What shipped is the generated-variants option, spelled as optional blocks:

```sql
-- name: FindUsers :many
SELECT id, email, status, balance
FROM users
WHERE organization_id = :org
  /*? AND email ILIKE :email */
  /*? AND status = :status */
ORDER BY email
LIMIT :limit;
```

Each block's parameters become optional labelled arguments; passing one
includes its block. Every inclusion combination is rewritten at codegen and
verified against Postgres with Describe, so the planner sees plain predicates
it can index, and the runtime never assembles a SQL string the generator has
not checked. The combinatorics are capped (4 blocks, 16 variants), all
variants must agree on the result shape, and block parameters cannot also be
nullable.

Rejected along the way: `(:email IS NULL OR email = :email)` — correct, but
PostgreSQL cannot use an index on a predicate it cannot see through — and
typed fragments, which are a query builder in disguise. The variant cap is
what keeps the combinatorial approach honest.

## 7. Bulk COPY inserts — done

`-- name: BulkAddUsers :copy` over a plain `INSERT INTO t (cols) VALUES
(:params)`. The INSERT is what Describe verifies (keeping the
everything-is-verified invariant); the generator builds `COPY t (cols) FROM
STDIN` from the verified column list, and the generated function takes a row
list and streams it in one round-trip through `PQputCopyData`. The runtime owns
COPY text escaping (`Value.Copy`, a third quoting regime after scalars and
array literals). libpq-only: the Caqti driver returns a clear error naming the
fix, because Caqti has no raw COPY surface and a row-at-a-time emulation would
silently lose COPY's all-or-nothing semantics.

## Not planned

- SQLite. No Describe equivalent, so inference needs a wholly different
  strategy, and its flexible typing makes the guarantee weaker anyway.
- Migrations. A separate concern with good existing tools.
- A query DSL. The premise of this project is that SQL is the source of truth.
