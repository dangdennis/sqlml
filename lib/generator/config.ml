(* See config.mli; the accepted file format is documented in the README. *)

type custom = { ocaml : string; of_string : string; to_string : string }

type t = {
  rename : (string * string) list;
      (** ["users"] or ["users.display_name"] to its replacement *)
  customs : (string * custom) list;
      (** ["users.id"], or a bare Postgres type name like ["citext"] *)
  schema : string list;
      (** schema files (relative to the queries directory) hashed into the offline
          snapshot, so [check --offline] can tell the snapshot is stale *)
}

let empty = { rename = []; customs = []; schema = [] }
let filename = "sqlml.toml"

open Gen_util

let err fmt = Diag.error fmt

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
      let customs =
        fold_result
          (fun acc k -> parse_custom toml k |> Result.map (fun c -> c :: acc))
          [] keys
        |> Result.map List.rev
      in
      let* customs = customs in
      let* schema =
        match Otoml.find_opt toml (Otoml.get_array Otoml.get_string) [ "schema" ] with
        | None -> Ok []
        | Some files -> Ok files
        | exception _ -> err "`schema`: expected an array of file paths"
      in
      Ok { rename; customs; schema }

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
let schema t = t.schema

(** Most specific wins: an exact ["table.column"] or ["Query.param"] key before a bare
    Postgres type name like ["uuid"].

    Parameters need their own keys because PostgreSQL reports a parameter's type but not
    which column it is compared against, so ["users.id"] cannot reach the [$1] in
    [WHERE id = $1]. Key those on the query name instead: ["GetUser.id"]. *)
let custom t ~key ~pg_type =
  match Option.bind key (fun k -> List.assoc_opt k t.customs) with
  | Some _ as c -> c
  | None -> List.assoc_opt pg_type t.customs

let qualify t ~names =
  let names = List.sort_uniq String.compare names in
  let short s =
    match String.index_opt s '.' with
    | None -> s
    | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  in
  let expand entries =
    fold_result
      (fun acc (key, v) ->
        if List.mem key names then Ok ((key, v) :: acc)
        else
          match List.filter (fun n -> short n = key) names with
          | [] -> Ok ((key, v) :: acc)
          | [ name ] ->
              if List.mem_assoc name entries then Ok acc else Ok ((name, v) :: acc)
          | candidates ->
              Diag.error "ambiguous configuration key %S: use one of %s" key
                (String.concat ", " candidates))
      [] entries
  in
  let* rename = expand t.rename in
  let* customs = expand t.customs in
  Ok { t with rename; customs }
