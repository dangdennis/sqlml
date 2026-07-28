(* Positional row decoders used by generated code.

   Generated decoders read like the SELECT list they came from:

     let decode r =
       { id          = Row.int r 0;
         email       = Row.string r 1;
         created_at  = Row.ptime r 2;
         display_name = Row.(option string) r 3 }

   Decoders raise [Bad] rather than returning a result so generated code stays
   flat; [Exec] catches it and turns it into [Error.Decode]. *)

type t = Value.t array

exception Bad of { column : int; expected : string; got : string }

let bad column expected v = raise (Bad { column; expected; got = Value.type_name v })

let get (r : t) i =
  if i < Array.length r then r.(i)
  else raise (Bad { column = i; expected = "a column"; got = "row of fewer columns" })

let int r i = match get r i with Value.Int n -> n | v -> bad i "int" v
let bool r i = match get r i with Value.Bool b -> b | v -> bad i "bool" v
let string r i = match get r i with Value.Text s -> s | v -> bad i "text" v
let octets r i = match get r i with Value.Octets s | Value.Text s -> s | v -> bad i "octets" v

let float r i =
  match get r i with
  | Value.Float f -> f
  | Value.Int n -> float_of_int n
  | v -> bad i "float" v

(* [option] wraps another decoder: [Row.(option string) r 3] *)
let option decode r i = match get r i with Value.Null -> None | _ -> Some (decode r i)
