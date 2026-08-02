(** The backend boundary: everything above is backend-neutral, everything below is a
    driver (libpq, Caqti). Generated code never names a driver — it receives a {!t}, which
    packs the driver away existentially. This module is driver SPI, not application API.
*)

type error = {
  message : string;
  sqlstate : Sqlstate.t option;
  detail : string option;
  hint : string option;
  constraint_name : string option;
  table_name : string option;
  column_name : string option;
}
(** What a driver reports when a statement fails. [sqlstate] is absent for client-side
    failures, where the server never classified anything. *)

val error :
  ?sqlstate:Sqlstate.t ->
  ?detail:string ->
  ?hint:string ->
  ?constraint_name:string ->
  ?table_name:string ->
  ?column_name:string ->
  string ->
  error

module type S = sig
  type conn

  val query :
    conn ->
    sql:string ->
    params:Value.t list ->
    columns:int ->
    (Value.t array list, error) result
  (** [columns] is how many columns the caller expects; drivers that discover the shape
      from the result (libpq) may ignore it, drivers that must declare it up front (Caqti)
      need it. *)

  val exec : conn -> sql:string -> params:Value.t list -> (int, error) result
  val close : conn -> unit
end

(** A connection: the driver module, its handle, and the transaction-depth counter owned
    by [Sqlml.transaction] (0 outside any transaction). The constructor is public because
    the runtime pattern-matches it; applications should treat values of this type as
    opaque. *)
type t = Conn : (module S with type conn = 'c) * 'c * int ref -> t

val make : (module S with type conn = 'c) -> 'c -> t

val session_setup : string list
(** Statements every driver must run once per physical connection. The temporal decoders
    parse ISO output, so pinning DateStyle is a runtime invariant, not a per-driver
    preference. *)
