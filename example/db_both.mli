(** SHAPE C -- row types at top level, query modules also exported.

    The hybrid: the row type is defined at the top level of the module (so one
    [open Db_both] at the top of a file makes every row's fields accessible),
    and the query module is still exported (so the generic runtime still works).
    Costs a few more lines of generated signature than Shape B and gives up
    nothing. *)

type get_user_row =
  { id : string
  ; email : string
  ; display_name : string option
  }

module Get_user : sig
  type params = { id : string }

  (* [params :=] substitutes away the abstract field (we declared it above);
     [row =] must be an equation, not a substitution -- [:=] would delete
     [type row] from the signature and the module would stop matching ONE. *)
  include Sqlml.Query.ONE with type params := params and type row = get_user_row
end

(** Fetch a single user by id. *)
val get_user : Sqlml.conn -> id:string -> (get_user_row option, Sqlml.Error.t) result
