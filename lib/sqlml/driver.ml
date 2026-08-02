(* See driver.mli for the boundary story. One note lives here: the asymmetry
   with query dispatch. The *driver* is existential (a plain GADT + first-class
   module, so a connection is an ordinary value you can store or pass around),
   while the *query* is a modular explicit, so its params/row types can appear
   in the execution function's type. *)

(* What a driver reports when a statement fails. [sqlstate] is absent for
   client-side failures (a dropped socket, a malformed URI) where the server
   never got far enough to classify anything. *)
type error = {
  message : string;
  sqlstate : Sqlstate.t option;
  detail : string option;
  hint : string option;
  constraint_name : string option;
  table_name : string option;
  column_name : string option;
}

let error ?sqlstate ?detail ?hint ?constraint_name ?table_name ?column_name message =
  { message; sqlstate; detail; hint; constraint_name; table_name; column_name }

module type S = sig
  type conn

  (* [columns] is how many columns the caller expects. Drivers that discover the
     shape from the result (libpq) may ignore it; drivers that must declare it
     up front (Caqti) need it. *)
  val query :
    conn ->
    sql:string ->
    params:Value.t list ->
    columns:int ->
    (Value.t array list, error) result

  val exec : conn -> sql:string -> params:Value.t list -> (int, error) result
  val close : conn -> unit
  (* Releases the underlying handle. For pooled connections this is the
     driver's choice of return-to-pool or no-op; the pool owns the lifetime. *)
end

(* Statements every driver must run once per physical connection before it is
   used. The temporal decoders parse ISO output, so this is a runtime
   invariant, not a per-driver preference -- a driver that skips it corrupts
   every date and timestamp read on servers with a different DateStyle. *)
let session_setup = [ "SET datestyle TO ISO" ]

(* The [int ref] is transaction depth, owned by [Sqlml.transaction]: 0 outside
   any transaction, incremented per nesting level. It lives here so that the
   runtime can decide between BEGIN and SAVEPOINT without asking the driver,
   which could not answer uniformly anyway. *)
type t = Conn : (module S with type conn = 'c) * 'c * int ref -> t

let make (type c) (module D : S with type conn = c) (c : c) : t =
  Conn ((module D), c, ref 0)
