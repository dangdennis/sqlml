(** sqlml runtime.

    Generated code depends on this and nothing else. The three execution functions take
    the generated query module as a {i modular explicit}, so the module determines both
    the parameter type you supply and the result type you get back — [Q.params] and
    [Q.row] project straight out of the argument rather than escaping as type variables
    through a [with type] witness. *)

module Pg_array = Pg_array
module Range = Range
module Composite = Composite
module Value = Value
module Error = Error
module Row = Row
module Driver = Driver
module Query = Query
module Sqlstate = Sqlstate
module Interval = Interval

(** {1 Connections} *)

type conn = Driver.t

val close : conn -> unit
(** Releases the connection. After this, using the handle is an error. For pool-borrowed
    handles the pool owns the lifetime — do not call this inside [Pool.use]. *)

(** {1 Errors} *)

exception Sql_error of Error.t

val or_raise : ('a, Error.t) result -> 'a

(** {1 Executing queries}

    Call sites read [Sqlml.fetch_one (module Db.Get_user) conn { id }]. In practice the
    generator emits a wrapper per query, so applications never write the module argument.
*)

val fetch_one :
  (module Q : Query.ONE) -> conn -> Q.params -> (Q.row option, Error.t) result

val fetch_all :
  (module Q : Query.MANY) -> conn -> Q.params -> (Q.row list, Error.t) result

val exec : (module Q : Query.EXEC) -> conn -> Q.params -> (int, Error.t) result

val fetch_one_strict :
  (module Q : Query.ONE_STRICT) -> conn -> Q.params -> (Q.row, Error.t) result
(** For queries declared [:one!]: the row is returned directly, and its absence is an
    [Error.Cardinality] rather than [None]. *)

val copy : (module Q : Query.COPY) -> conn -> Q.params list -> (int, Error.t) result
(** For queries declared [:copy]: bulk-loads the rows through [COPY ... FROM STDIN] in one
    round-trip and returns the count written. All-or-nothing: any bad row aborts the whole
    COPY server-side. *)

(** {1 Streaming}

    For results too large to hold as a list. Rows are read in batches from a server-side
    cursor, so memory use is bounded by [~batch], not by the result size. Works through
    both drivers, and inside an enclosing transaction.

    The fold runs inside a transaction of its own (a cursor requires one); if [f] raises,
    the transaction rolls back and the exception is re-raised. *)

val fetch_fold :
  (module Q : Query.MANY) ->
  ?batch:int ->
  conn ->
  Q.params ->
  init:'acc ->
  f:('acc -> Q.row -> 'acc) ->
  ('acc, Error.t) result

val fetch_iter :
  (module Q : Query.MANY) ->
  ?batch:int -> conn -> Q.params -> f:(Q.row -> unit) -> (unit, Error.t) result

(** {1 Transactions} *)

val transaction :
  ?isolation:[ `Read_committed | `Repeatable_read | `Serializable ] ->
  ?retry:int ->
  conn ->
  (conn -> ('a, Error.t) result) ->
  ('a, Error.t) result
(** [transaction conn f] runs [f] inside a transaction: COMMIT when [f] returns [Ok],
    ROLLBACK when it returns [Error] or raises — the exception is re-raised after the
    rollback, so [_exn] query functions abort the transaction as you would expect. The
    handle passed to [f] has the same type as [conn], so every generated query function
    works inside unchanged.

    Nesting uses savepoints: a [transaction] inside a transaction rolls back only its own
    work on failure, and the outer transaction continues.

    [~isolation] sets the isolation level. It is only meaningful at the outermost level;
    inside an enclosing transaction it raises [Invalid_argument], because PostgreSQL
    cannot change isolation mid-flight.

    [~retry:n] re-runs the body up to [n] more times on a serialization failure ([40001])
    or deadlock ([40P01]) — the standard companion to [`Serializable]. Only those two
    codes retry; anything else would fail identically again. Retries happen at the
    outermost level only: PostgreSQL dooms the entire transaction on a serialization
    failure, so an inner error propagates out and the outer attempt re-runs everything.
    The body must be safe to re-run.

    {[
    Sqlml.transaction ~isolation:`Serializable ~retry:3 conn @@ fun tx ->
    let* balance = get_balance tx ~id in
    set_balance tx ~id ~balance:(debit balance amount)
    ]} *)
