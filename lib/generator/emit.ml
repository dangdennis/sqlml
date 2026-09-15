open Gen_util
(** Resolve all identities before rendering any declarations. *)

let generate ?(config = Config.empty) ~src (described : Describe.described list) =
  let types =
    List.concat_map
      (fun (d : Describe.described) ->
        List.map (fun (c : Describe.column) -> c.pg_type) d.columns
        @ List.map (fun (p : Describe.param) -> p.pg_type) d.params)
      described
  in
  let registry =
    List.concat_map (fun t -> t.Pg_type.registry) types |> List.sort_uniq compare
  in
  let names =
    List.concat_map
      (fun (id, k) ->
        Pg_type.qualified id
        ::
        (match k with
        | Pg_type.Composite attrs ->
            List.map
              (fun (a : Pg_type.attribute) -> Pg_type.qualified id ^ "." ^ a.name)
              attrs
        | _ -> []))
      registry
    @ List.concat_map
        (fun (d : Describe.described) ->
          List.concat_map
            (fun (c : Describe.column) ->
              match c.table with None -> [] | Some t -> [ t; t ^ "." ^ c.name ])
            d.columns)
        described
  in
  let* config = Config.qualify config ~names in
  let named =
    List.filter_map
      (fun (id, k) ->
        match k with
        | Pg_type.Enum _ | Pg_type.Composite _ -> Some (Resolve.rename_type config id, id)
        | _ -> None)
      registry
  in
  let rec unique seen = function
    | [] -> Ok ()
    | (name, id) :: xs -> (
        if not (Gen_util.is_lower_ident name) then
          Diag.error "type %s generates invalid name %S; rename it" (Pg_type.qualified id)
            name
        else
          match List.assoc_opt name seen with
          | Some prev when prev <> id ->
              Diag.error "types %s and %s generate the same name %S; rename one"
                (Pg_type.qualified prev) (Pg_type.qualified id) name
          | _ -> unique ((name, id) :: seen) xs)
  in
  let* () = unique [] named in
  let* resolved = map_result (Resolve.resolve config) described in
  let* rows = Resolve.collect_rows resolved in
  let* composites = Resolve.collect_composites config described in
  let* enums = Resolve.collect_enums resolved in
  let enums =
    List.sort_uniq compare
      (enums @ List.concat_map (fun (_, fs) -> Resolve.enums_in_fields fs) composites)
  in
  let* _ =
    map_result
      (fun (name, labels) ->
        let constructors = List.map Typemap.constructor_of_label labels in
        if
          List.length constructors
          <> List.length (List.sort_uniq String.compare constructors)
        then
          Diag.error
            "enum %s has labels that map to the same OCaml constructor; use a custom \
             mapping"
            name
        else Ok ())
      enums
  in
  let all_names = List.map fst enums @ List.map fst composites @ List.map fst rows in
  if List.length all_names <> List.length (List.sort_uniq String.compare all_names) then
    Diag.error "generated enum, composite, or row names collide; add a rename"
  else Ok (Render.source ~src ~enums ~composites ~rows resolved)
