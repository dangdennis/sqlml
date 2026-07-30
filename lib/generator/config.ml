(** Optional [sqlml.toml], read from the queries directory.

    Two things it controls, both of which have to be stated rather than guessed: what
    generated names are called, and which OCaml type a column maps to.

    {[
      # Rename generated types and fields.
      [rename]
      users = "user"                    # users_row becomes user_row
      "users.display_name" = "name"     # the field becomes `name`

      # Map a column, or a whole Postgres type, to your own OCaml type.
      [types."users.id"]
      ocaml = "User_id.t"
      of_string = "User_id.of_string"
      to_string = "User_id.to_string"

      [types.citext]
      ocaml = "Email.t"
      of_string = "Email.of_string"
      to_string = "Email.to_string"
    ]} *)

type custom = { ocaml : string; of_string : string; to_string : string }

type t = {
  rename : (string * string) list;
      (** ["users"] or ["users.display_name"] to its replacement *)
  customs : (string * custom) list;
      (** ["users.id"], or a bare Postgres type name like ["citext"] *)
}

let empty = { rename = []; customs = [] }
let filename = "sqlml.toml"
let err fmt = Diag.error fmt

let rec fold_result f acc = function
  | [] -> Ok acc
  | x :: tl -> ( match f acc x with Ok acc -> fold_result f acc tl | Error _ as e -> e)

let parse_custom toml key =
  let get f = Otoml.find_opt toml Otoml.get_string [ "types"; key; f ] in
  match (get "ocaml", get "of_string", get "to_string") with
  | Some ocaml, Some of_string, Some to_string -> Ok (key, { ocaml; of_string; to_string })
  | None, _, _ -> err "[types.%S]: missing `ocaml`" key
  | _, None, _ -> err "[types.%S]: missing `of_string`" key
  | _, _, None -> err "[types.%S]: missing `to_string`" key

let of_toml toml =
  let rename =
    match Otoml.find_opt toml Otoml.get_table [ "rename" ] with
    | None -> Ok []
    | Some entries ->
        fold_result
          (fun acc (k, v) ->
            match Otoml.get_string v with
            | s -> Ok ((k, s) :: acc)
            | exception _ -> err "[rename] %S: expected a string" k)
          [] entries
        |> Result.map List.rev
  in
  match rename with
  | Error _ as e -> e
  | Ok rename ->
      let keys =
        match Otoml.find_opt toml Otoml.list_table_keys [ "types" ] with
        | None -> []
        | Some ks -> ks
      in
      fold_result
        (fun acc k -> parse_custom toml k |> Result.map (fun c -> c :: acc))
        [] keys
      |> Result.map (fun customs -> { rename; customs = List.rev customs })

let load dir =
  let path = Filename.concat dir filename in
  if not (Sys.file_exists path) then Ok empty
  else
    match Otoml.Parser.from_file_result path with
    | Error m -> Diag.error ~file:path "%s" m
    | Ok toml -> (
        match of_toml toml with
        | Ok c -> Ok c
        | Error (e : Diag.t) -> Error { e with Diag.file = Some path })

(* ---------- lookups ---------- *)

let renamed t key = List.assoc_opt key t.rename

(** Most specific wins: an exact ["table.column"] or ["Query.param"] key before a bare
    Postgres type name like ["uuid"].

    Parameters need their own keys because PostgreSQL reports a parameter's type but not
    which column it is compared against, so ["users.id"] cannot reach the [$1] in
    [WHERE id = $1]. Key those on the query name instead: ["GetUser.id"]. *)
let custom t ~key ~pg_type =
  match Option.bind key (fun k -> List.assoc_opt k t.customs) with
  | Some _ as c -> c
  | None -> List.assoc_opt pg_type t.customs
