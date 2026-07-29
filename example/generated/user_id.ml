(* A domain type, to show sqlml.toml mapping a column onto it. *)
type t = Uuidm.t

let of_string s =
  match Uuidm.of_string s with Some u -> u | None -> failwith "User_id.of_string"

let to_string = Uuidm.to_string
let equal = Uuidm.equal
