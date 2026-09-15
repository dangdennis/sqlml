type id = { schema : string; name : string }
type attribute = { name : string; typ : id; number : int; typmod : int }

type kind =
  | Base
  | Enum of string list
  | Domain of { base : id; not_null : bool; typmod : int; constraints : string list }
  | Array of { element : id; delimiter : char }
  | Composite of attribute list
  | Range of id
  | Multirange of id
  | Unsupported of string

type registry = (id * kind) list
type t = { id : id; registry : registry }

let qualified id =
  let quote s =
    if
      s <> ""
      && String.for_all
           (fun c -> (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_')
           s
      && not (s.[0] >= '0' && s.[0] <= '9')
    then s
    else "\"" ^ String.concat "\"\"" (String.split_on_char '\"' s) ^ "\""
  in
  quote id.schema ^ "." ^ quote id.name

let generated_name id = Parse.to_snake (id.schema ^ "_" ^ id.name)
let kind t = List.assoc t.id t.registry
let at t id = { t with id }

let refs = function
  | Domain d -> [ d.base ]
  | Array a -> [ a.element ]
  | Composite a -> List.map (fun a -> a.typ) a
  | Range r | Multirange r -> [ r ]
  | Base | Enum _ | Unsupported _ -> []

let legacy ~name ~element ~labels =
  let id name = { schema = (if labels = [] then "pg_catalog" else "public"); name } in
  let root = id name in
  let scalar = if labels = [] then Base else Enum labels in
  let registry =
    match element with
    | None -> [ (root, scalar) ]
    | Some e -> [ (root, Array { element = id e; delimiter = ',' }); (id e, scalar) ]
  in
  { id = root; registry }

let discover conn roots =
  let ( let* ) = Result.bind in
  let query s =
    Result.map_error (fun (e : Pq.diag) -> Diag.v e.message) (Pq.query conn s)
  in
  let ids = Hashtbl.create 32 and nodes = Hashtbl.create 32 in
  let identity oid =
    match Hashtbl.find_opt ids oid with
    | Some x -> Ok x
    | None -> (
        let* rows =
          query
            (Printf.sprintf
               "select n.nspname,t.typname from pg_catalog.pg_type t join \
                pg_catalog.pg_namespace n on n.oid=t.typnamespace where t.oid=%d"
               oid)
        in
        match rows with
        | [ r ] ->
            let id = { schema = r.(0); name = r.(1) } in
            Hashtbl.add ids oid id;
            Ok id
        | _ -> Diag.error "catalog: missing PostgreSQL type OID %d" oid)
  in
  let rec visit oid =
    let* id = identity oid in
    if Hashtbl.mem nodes oid then Ok id
    else begin
      (* Mark before descending: a named dependency must not recurse forever. *)
      Hashtbl.add nodes oid (Unsupported "incomplete catalog type");
      let* rows =
        query
          (Printf.sprintf
             "select \
              t.typtype,t.typbasetype,t.typnotnull,t.typtypmod,t.typrelid,t.typelem,t.typdelim,t.typsubscript \
              = 'pg_catalog.array_subscript_handler'::regproc AND EXISTS (SELECT 1 FROM \
              pg_catalog.pg_type e WHERE e.oid=t.typelem AND e.typarray=t.oid) from \
              pg_catalog.pg_type t where t.oid=%d"
             oid)
      in
      let* k =
        match rows with
        | [ r ] ->
            begin match r.(0) with
            | "d" ->
                let* base = visit (int_of_string r.(1)) in
                let* cs =
                  query
                    (Printf.sprintf
                       "select pg_catalog.pg_get_constraintdef(oid) from \
                        pg_catalog.pg_constraint where contypid=%d order by conname"
                       oid)
                in
                Ok
                  (Domain
                     {
                       base;
                       not_null = r.(2) = "t";
                       typmod = int_of_string r.(3);
                       constraints = List.map (fun x -> x.(0)) cs;
                     })
            | "e" ->
                let* rs =
                  query
                    (Printf.sprintf
                       "select enumlabel from pg_catalog.pg_enum where enumtypid=%d \
                        order by enumsortorder"
                       oid)
                in
                Ok (Enum (List.map (fun x -> x.(0)) rs))
            | "c" ->
                let* rs =
                  query
                    (Printf.sprintf
                       "select attname,atttypid,attnum,atttypmod from \
                        pg_catalog.pg_attribute where attrelid=%s and attnum>0 and not \
                        attisdropped order by attnum"
                       r.(4))
                in
                let* attrs =
                  Gen_util.map_result
                    (fun a ->
                      let* typ = visit (int_of_string a.(1)) in
                      Ok
                        {
                          name = a.(0);
                          typ;
                          number = int_of_string a.(2);
                          typmod = int_of_string a.(3);
                        })
                    rs
                in
                Ok (Composite attrs)
            | ("r" | "m") as tag -> (
                let* rs =
                  query
                    (Printf.sprintf
                       "select rngsubtype,rngtypid from pg_catalog.pg_range where %s=%d"
                       (if tag = "r" then "rngtypid" else "rngmultitypid")
                       oid)
                in
                match rs with
                | [ a ] ->
                    let* child = visit (int_of_string a.(if tag = "r" then 0 else 1)) in
                    Ok (if tag = "r" then Range child else Multirange child)
                | _ -> Diag.error "catalog: missing range %s" (qualified id))
            | "b" when r.(7) = "t" && r.(5) <> "0" -> (
                let elem_oid = int_of_string r.(5) in
                let* element = visit elem_oid in
                let* rs =
                  query
                    (Printf.sprintf "select typdelim from pg_catalog.pg_type where oid=%d"
                       elem_oid)
                in
                match rs with
                | [ a ] -> Ok (Array { element; delimiter = a.(0).[0] })
                | _ -> Diag.error "catalog: missing array delimiter")
            | "b" -> Ok Base
            | tag -> Ok (Unsupported tag)
            end
        | _ -> Diag.error "catalog: missing type %s" (qualified id)
      in
      Hashtbl.replace nodes oid k;
      Ok id
    end
  in
  let* _ = Gen_util.map_result visit (List.sort_uniq compare roots) in
  let registry =
    Hashtbl.fold (fun oid k xs -> (Hashtbl.find ids oid, k) :: xs) nodes []
    |> List.sort compare
  in
  Ok (List.map (fun oid -> (oid, { id = Hashtbl.find ids oid; registry })) roots)

let json_id id = `List [ `String id.schema; `String id.name ]

let to_json t =
  let kind = function
    | Base -> `List [ `String "base" ]
    | Enum xs -> `List [ `String "enum"; `List (List.map (fun s -> `String s) xs) ]
    | Domain d ->
        `List
          [
            `String "domain";
            json_id d.base;
            `Bool d.not_null;
            `Int d.typmod;
            `List (List.map (fun s -> `String s) d.constraints);
          ]
    | Array a ->
        `List [ `String "array"; json_id a.element; `String (String.make 1 a.delimiter) ]
    | Composite xs ->
        `List
          [
            `String "composite";
            `List
              (List.map
                 (fun a ->
                   `List [ `String a.name; json_id a.typ; `Int a.number; `Int a.typmod ])
                 xs);
          ]
    | Range r -> `List [ `String "range"; json_id r ]
    | Multirange r -> `List [ `String "multirange"; json_id r ]
    | Unsupported s -> `List [ `String "unsupported"; `String s ]
  in
  `Assoc
    [
      ("root", json_id t.id);
      ( "types",
        `List
          (List.map
             (fun (id, k) -> `List [ json_id id; kind k ])
             (List.sort compare t.registry)) );
    ]

let of_json j =
  let bad () = invalid_arg "invalid PostgreSQL type graph" in
  let id = function
    | `List [ `String schema; `String name ] -> { schema; name }
    | _ -> bad ()
  in
  let strings = function
    | `List xs -> List.map (function `String s -> s | _ -> bad ()) xs
    | _ -> bad ()
  in
  let kind = function
    | `List [ `String "base" ] -> Base
    | `List [ `String "enum"; xs ] -> Enum (strings xs)
    | `List [ `String "domain"; b; `Bool not_null; `Int typmod; cs ] ->
        Domain { base = id b; not_null; typmod; constraints = strings cs }
    | `List [ `String "array"; e; `String d ] when String.length d = 1 ->
        Array { element = id e; delimiter = d.[0] }
    | `List [ `String "composite"; `List xs ] ->
        Composite
          (List.map
             (function
               | `List [ `String name; t; `Int number; `Int typmod ] ->
                   { name; typ = id t; number; typmod }
               | _ -> bad ())
             xs)
    | `List [ `String "range"; r ] -> Range (id r)
    | `List [ `String "multirange"; r ] -> Multirange (id r)
    | `List [ `String "unsupported"; `String s ] -> Unsupported s
    | _ -> bad ()
  in
  match j with
  | `Assoc xs ->
      let root = try id (List.assoc "root" xs) with Not_found -> bad () in
      let registry =
        match List.assoc_opt "types" xs with
        | Some (`List xs) ->
            List.map (function `List [ i; k ] -> (id i, kind k) | _ -> bad ()) xs
        | _ -> bad ()
      in
      let keys = List.map fst registry in
      if
        List.length keys <> List.length (List.sort_uniq compare keys)
        || (not (List.mem root keys))
        || List.exists
             (fun (_, k) -> List.exists (fun id -> not (List.mem id keys)) (refs k))
             registry
      then bad ();
      { id = root; registry }
  | _ -> bad ()
