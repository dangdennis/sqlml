(** Thin binding over libpq, shared by the generator (PQdescribePrepared,
    PQftable/PQftablecol — the basis of nullability and shared-model detection) and the
    libpq driver (PQexecParams / prepared execution).

    [conn] and [res] are abstract on purpose: both are raw C pointers, and when they were
    aliases of the same type, [finish] on a result or [clear] on a connection typechecked
    and corrupted memory. Lifetimes are the caller's: every
    [prepare]/[describe_prepared]/[exec]/[exec_params]/[exec_prepared] result must be
    [clear]ed (or go through {!with_result}/{!check}), and every [connect]ed handle must
    be [finish]ed. *)

type conn
type res

type diag = {
  message : string;
  sqlstate : string option;
  detail : string option;
  hint : string option;
  constraint_name : string option;
  table_name : string option;
  column_name : string option;
}
(** Diagnostics collected off a failed result before it is cleared. *)

(** {1 Connections} *)

val connect : string -> conn
val connect_ok : conn -> bool
val error_message : conn -> string
val finish : conn -> unit

val conninfo_of_env : unit -> string
(** [DATABASE_URL], or a keyword string from [PGHOST]/[PGPORT]/[PGUSER]/
    [PGDATABASE]/[PGPASSWORD]. *)

(** {1 Executing} *)

val prepare : conn -> string -> string -> res
val describe_prepared : conn -> string -> res
val exec : conn -> string -> res
val exec_params : conn -> string -> string option array -> res
val exec_prepared : conn -> string -> string option array -> res

(** {1 Results} *)

val ok : int -> bool
val result_status : res -> int
val clear : res -> unit

val check : res -> (res, diag) result
(** [Ok] passes the live result through; on failure the diagnostics are read and the
    result is cleared. *)

val with_result : res -> (res -> 'a) -> 'a
(** Runs the function and always clears, even on raise. *)

val opt_field : res -> int -> string option
val diag_sqlstate : int
val nparams : res -> int
val paramtype : res -> int -> int
val nfields : res -> int
val fname : res -> int -> string
val ftype : res -> int -> int
val ftable : res -> int -> int
val ftablecol : res -> int -> int
val ntuples : res -> int
val getvalue : res -> int -> int -> string
val getisnull : res -> int -> int -> bool
val cmd_tuples : res -> int

val query : conn -> string -> (string array list, diag) result
(** A catalog query returning rows as string arrays; NULL becomes [""]. *)

val copy_from : conn -> sql:string -> rows:string list -> (int, diag) result
(** The whole [COPY ... FROM STDIN] conversation: enter copy-in mode, stream the
    pre-escaped text lines (no trailing newline), end, and drain every pending result so
    the connection stays usable even when the server aborts mid-stream. Returns rows
    written. *)
