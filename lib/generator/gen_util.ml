(** Result plumbing shared across the generator. *)

let ( let* ) = Result.bind

let rec map_result f = function
  | [] -> Ok []
  | x :: tl ->
      let* y = f x in
      let* rest = map_result f tl in
      Ok (y :: rest)

let rec fold_result f acc = function
  | [] -> Ok acc
  | x :: tl -> ( match f acc x with Ok acc -> fold_result f acc tl | Error _ as e -> e)
