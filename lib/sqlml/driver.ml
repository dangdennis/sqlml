(* The backend boundary.

   Everything above this line is backend-neutral; everything below is a driver
   (sqlml_caqti today, a native wire driver later). Generated code never names a
   driver -- it receives a [Driver.t], which packs the driver away existentially.

   Note the asymmetry with query dispatch: the *driver* is existential (a plain
   GADT + first-class module, so a connection is an ordinary value you can store
   in a record or pass around), while the *query* is a modular explicit, so its
   params/row types can appear in the execution function's type. *)

module type S = sig
  type conn

  val name : string

  (* Dialect placeholder for the n-th parameter, 1-indexed: "$1" / "?" *)
  val placeholder : int -> string

  val query :
    conn -> sql:string -> params:Value.t list -> (Value.t array list, string) result

  val exec : conn -> sql:string -> params:Value.t list -> (int, string) result
end

type t = Conn : (module S with type conn = 'c) * 'c -> t

let make (type c) (module D : S with type conn = c) (c : c) : t = Conn ((module D), c)
let name (Conn ((module D), _)) = D.name
