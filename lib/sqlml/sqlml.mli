(** sqlml runtime.

    Generated code depends on this and nothing else. The three execution
    functions take the generated query module as a {i modular explicit}, so the
    module determines both the parameter type you supply and the result type you
    get back — [Q.params] and [Q.row] project straight out of the argument
    rather than escaping as type variables through a [with type] witness. *)

module Value = Value
module Error = Error
module Row = Row
module Driver = Driver
module Query = Query
module Sqlstate = Sqlstate

(** {1 Connections} *)

(** An open connection. Obtained from a driver, e.g. [Sqlml_pg.connect] or [Sqlml_caqti.Pool.use]. *)
type conn = Driver.t

(** {1 Errors} *)

exception Sql_error of Error.t

val or_raise : ('a, Error.t) result -> 'a

(** {1 Executing queries}

    Call sites read [Sqlml.fetch_one (module Db.Get_user) conn { id }]. In practice the
    generator emits a wrapper per query, so applications never write the module argument. *)

val fetch_one : (module Q : Query.ONE) -> conn -> Q.params -> (Q.row option, Error.t) result
val fetch_all : (module Q : Query.MANY) -> conn -> Q.params -> (Q.row list, Error.t) result
val exec : (module Q : Query.EXEC) -> conn -> Q.params -> (int, Error.t) result

(** {1 Transactions} *)

(** [transaction conn f] runs [f] inside BEGIN/COMMIT. It commits when [f]
    returns [Ok], and rolls back when [f] returns [Error] or raises — an
    exception is re-raised after the rollback, so [_exn] query functions abort
    the transaction as you would expect.

    The handle passed to [f] has the same type as [conn], so every generated
    query function works inside a transaction unchanged.

    {b Known gap.} The intent was to give [f] a distinct [tx conn], phantom
    tagged, so that a nested transaction was a type error and an outer handle
    could not be used inside the body. That is not expressible here: the tag
    does not generalise through the modular-explicit query functions, and a
    binding whose type contains a modular-explicit arrow cannot carry an
    explicit polymorphic annotation at all — both ['k.] and [type k.] are
    rejected with "the universal variable would escape its scope". With a single
    connection nothing is actually unsafe: the outer handle {i is} the same
    connection, so a statement issued through it is still inside the
    transaction. The exposure appears only with pooling, and the fix there needs
    no phantom types — make the pool a distinct type carrying no query
    operations, so that obtaining a connection requires going through this
    function. *)
val transaction : conn -> (conn -> ('a, Error.t) result) -> ('a, Error.t) result
