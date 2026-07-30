(** Parsing .sql files into named queries.

    Authoring model is sqlc's: many queries per file, each introduced by a header comment
    carrying a name and a cardinality.

    -- name: GetUser :one -- Fetches a single user by id. SELECT id, email, display_name
    FROM users WHERE id = :id;

    Named parameters (:id) are rewritten to positional ($1) before the query is sent to
    Postgres, and the names become the fields of the generated params record.

    Nullability overrides are deliberately NOT handled here. They ride to Postgres inside
    a quoted column alias --

    SELECT coalesce(total, 0) AS "total!"

    -- so Postgres echoes the marker back in RowDescription and [Describe] strips it. That
    means we never rewrite the user's SELECT list, and a marker can never be confused with
    SQL syntax. *)

type cardinality = One | One_strict | Many | Exec

(* [nullable] comes from a trailing ? on the placeholder -- [:display_name?].
   It cannot be inferred: Postgres's Describe reports parameter types but says
   nothing about whether a parameter may be null, so this has to be stated. *)
type param = { pname : string; index : int; nullable : bool }

(* A query containing /*? ... */ optional blocks. [variant_sqls] holds the
   assembled, $n-rewritten SQL for every inclusion combination, indexed by
   bitmask (bit k = block k included); [block_params] names each block's
   parameters. The query's main [sql]/[params] are the full variant (all
   blocks included), which is the one that mentions every parameter. *)
type dyn = {
  nblocks : int;
  variant_sqls : string array;
  variant_nparams : int array;
  block_params : string list array;
}

type t = {
  name : string; (* GetUser, as written *)
  module_name : string; (* Get_user *)
  cardinality : cardinality;
  doc : string list;
  sql : string; (* with $1 placeholders *)
  params : param list; (* ordered by index *)
  dynamic : dyn option;
  file : string;
  line : int;
}

let err file line fmt = Diag.error ~file ~line fmt

(* ---------- identifiers ---------- *)

let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9')
let is_upper c = c >= 'A' && c <= 'Z'
let is_lower c = c >= 'a' && c <= 'z'
let is_digit c = c >= '0' && c <= '9'

(* GetUser -> get_user, SearchUsersByOrg -> search_users_by_org, GetUserV2 -> get_user_v2 *)
let to_snake s =
  let n = String.length s in
  let b = Buffer.create (n + 4) in
  for i = 0 to n - 1 do
    let c = s.[i] in
    if is_upper c then begin
      let prev_lower_or_digit = i > 0 && (is_lower s.[i - 1] || is_digit s.[i - 1]) in
      let boundary_of_acronym =
        i > 0 && is_upper s.[i - 1] && i + 1 < n && is_lower s.[i + 1]
      in
      if prev_lower_or_digit || boundary_of_acronym then Buffer.add_char b '_';
      Buffer.add_char b (Char.lowercase_ascii c)
    end
    else Buffer.add_char b c
  done;
  Buffer.contents b

let to_module_name s =
  let snake = to_snake s in
  if snake = "" then snake
  else
    String.make 1 (Char.uppercase_ascii snake.[0])
    ^ String.sub snake 1 (String.length snake - 1)

(* ---------- named parameters -> $n ---------- *)

(* A single pass that copies SQL verbatim except for :name, which becomes $n.
   Everything that can legally contain a colon is skipped over: string literals,
   quoted identifiers, line and block comments, dollar-quoted bodies, and the
   :: cast operator. Getting :: wrong would silently corrupt `id::text`. *)
let rewrite_params ~file ~line sql =
  let n = String.length sql in
  let buf = Buffer.create n in
  let order = ref [] in
  let next_index = ref 0 in
  let bad = ref None in
  let saw_positional = ref false in
  let i = ref 0 in
  let copy_char () =
    Buffer.add_char buf sql.[!i];
    incr i
  in
  (* copy through a terminator, honouring doubled-quote escapes *)
  let copy_quoted q =
    copy_char ();
    let fin = ref false in
    while (not !fin) && !i < n do
      if sql.[!i] = q then
        if !i + 1 < n && sql.[!i + 1] = q then (
          copy_char ();
          copy_char ())
        else (
          copy_char ();
          fin := true)
      else copy_char ()
    done;
    if not !fin then bad := Some (Printf.sprintf "unterminated %c" q)
  in
  while !i < n do
    let c = sql.[!i] in
    if c = '\'' then copy_quoted '\''
    else if c = '"' then copy_quoted '"'
    else if c = '-' && !i + 1 < n && sql.[!i + 1] = '-' then
      while !i < n && sql.[!i] <> '\n' do
        copy_char ()
      done
    else if c = '/' && !i + 1 < n && sql.[!i + 1] = '*' then begin
      (* block comments nest in Postgres *)
      let depth = ref 0 in
      let fin = ref false in
      while (not !fin) && !i < n do
        if !i + 1 < n && sql.[!i] = '/' && sql.[!i + 1] = '*' then (
          incr depth;
          copy_char ();
          copy_char ())
        else if !i + 1 < n && sql.[!i] = '*' && sql.[!i + 1] = '/' then begin
          decr depth;
          copy_char ();
          copy_char ();
          if !depth = 0 then fin := true
        end
        else copy_char ()
      done;
      if not !fin then bad := Some "unterminated /* comment"
    end
    else if c = '$' && !i + 1 < n && is_digit sql.[!i + 1] then begin
      (* an explicit positional placeholder the user wrote; leave it alone *)
      saw_positional := true;
      copy_char ()
    end
    else if c = '$' && !i + 1 < n && (sql.[!i + 1] = '$' || is_ident_start sql.[!i + 1])
    then begin
      (* dollar-quoted string: $tag$ ... $tag$ *)
      let start = !i in
      let j = ref (!i + 1) in
      while !j < n && is_ident_char sql.[!j] do
        incr j
      done;
      if !j < n && sql.[!j] = '$' then begin
        let tag = String.sub sql start (!j - start + 1) in
        let tlen = String.length tag in
        Buffer.add_string buf tag;
        i := !j + 1;
        let fin = ref false in
        while (not !fin) && !i < n do
          if !i + tlen <= n && String.sub sql !i tlen = tag then begin
            Buffer.add_string buf tag;
            i := !i + tlen;
            fin := true
          end
          else copy_char ()
        done;
        if not !fin then bad := Some ("unterminated " ^ tag ^ " string")
      end
      else copy_char ()
    end
    else if c = ':' && !i + 1 < n && sql.[!i + 1] = ':' then (
      copy_char ();
      copy_char ())
    else if c = ':' && !i + 1 < n && is_ident_start sql.[!i + 1] then begin
      let start = !i + 1 in
      let j = ref start in
      while !j < n && is_ident_char sql.[!j] do
        incr j
      done;
      let name = String.sub sql start (!j - start) in
      (* trailing ? marks the parameter nullable, and is not part of the SQL *)
      let nullable = !j < n && sql.[!j] = '?' in
      if nullable then incr j;
      let index =
        match List.assoc_opt name !order with
        | Some (k, was_null) ->
            order := (name, (k, was_null || nullable)) :: List.remove_assoc name !order;
            k
        | None ->
            incr next_index;
            order := (name, (!next_index, nullable)) :: !order;
            !next_index
      in
      Buffer.add_string buf ("$" ^ string_of_int index);
      i := !j
    end
    else copy_char ()
  done;
  match !bad with
  | Some m -> err file line "%s" m
  (* Named parameters are numbered from $1 by our own counter, so mixing them
     with explicit $n placeholders would silently collide -- :b in
     "WHERE a = $1 AND b = :b" would also become $1. Use one style. *)
  | None when !saw_positional && !next_index > 0 ->
      err file line "mix of named (:name) and positional ($n) parameters; use one style"
  | None ->
      let params =
        !order
        |> List.rev_map (fun (pname, (index, nullable)) -> { pname; index; nullable })
        |> List.sort (fun a b -> compare a.index b.index)
      in
      Ok (Buffer.contents buf, params)

(* ---------- optional blocks ---------- *)

let max_blocks = 4

(* Split raw SQL into fixed text and /*? ... */ optional blocks. *)
let split_blocks ~file ~line raw =
  let n = String.length raw in
  let segs = ref [] in
  let buf = Buffer.create n in
  let i = ref 0 in
  let flush () =
    if Buffer.length buf > 0 then begin
      segs := `Fixed (Buffer.contents buf) :: !segs;
      Buffer.clear buf
    end
  in
  let bad = ref None in
  while !i < n && !bad = None do
    if !i + 2 < n && raw.[!i] = '/' && raw.[!i + 1] = '*' && raw.[!i + 2] = '?' then begin
      let rec find j =
        if j + 1 >= n then None
        else if raw.[j] = '*' && raw.[j + 1] = '/' then Some j
        else find (j + 1)
      in
      match find (!i + 3) with
      | None -> bad := Some "unterminated /*? ... */ block"
      | Some j ->
          flush ();
          segs := `Block (String.trim (String.sub raw (!i + 3) (j - !i - 3))) :: !segs;
          i := j + 2
    end
    else begin
      Buffer.add_char buf raw.[!i];
      incr i
    end
  done;
  match !bad with
  | Some m -> err file line "%s" m
  | None ->
      flush ();
      Ok (List.rev !segs)

(* Assemble the variant for [mask] and rewrite its named parameters. A space
   joins segments so an included block never fuses with surrounding text. *)
let assemble ~file ~line segs mask =
  let b = Buffer.create 256 in
  let k = ref 0 in
  List.iter
    (fun seg ->
      match seg with
      | `Fixed t -> Buffer.add_string b t
      | `Block t ->
          if mask land (1 lsl !k) <> 0 then begin
            Buffer.add_char b ' ';
            Buffer.add_string b t;
            Buffer.add_char b ' '
          end;
          incr k)
    segs;
  rewrite_params ~file ~line (Buffer.contents b)

let names ps = List.map (fun p -> p.pname) ps

(* Blocks -> variants, with the rules that keep runtime selection sound:
   every block has at least one parameter; a block's parameters appear in no
   other block and not in the fixed text; no `?` nullable marker inside a
   block (a block parameter is already optional). *)
let build_dynamic ~file ~line segs =
  let nblocks =
    List.length (List.filter (function `Block _ -> true | _ -> false) segs)
  in
  if nblocks > max_blocks then
    err file line
      "%d optional blocks; at most %d are supported (%d variants each need a describe \
       round-trip)"
      nblocks max_blocks (1 lsl max_blocks)
  else begin
    let nvariants = 1 lsl nblocks in
    let variants = Array.make nvariants ("", []) in
    let bad = ref None in
    (try
       for mask = 0 to nvariants - 1 do
         match assemble ~file ~line segs mask with
         | Ok v -> variants.(mask) <- v
         | Error e ->
             bad := Some e;
             raise Exit
       done
     with Exit -> ());
    match !bad with
    | Some e -> Error e
    | None -> (
        let base_names = names (snd variants.(0)) in
        let block_params = Array.make nblocks [] in
        let problem = ref None in
        for k = 0 to nblocks - 1 do
          let only_k = names (snd variants.(1 lsl k)) in
          let bp = List.filter (fun n -> not (List.mem n base_names)) only_k in
          block_params.(k) <- bp;
          if bp = [] && !problem = None then
            problem :=
              Some
                (Printf.sprintf "optional block %d has no parameters of its own" (k + 1))
        done;
        (* disjointness across blocks *)
        let all_bp = Array.to_list block_params |> List.concat in
        if
          List.length all_bp <> List.length (List.sort_uniq compare all_bp)
          && !problem = None
        then problem := Some "a parameter appears in more than one optional block";
        (* no nullable markers inside blocks *)
        let full = snd variants.(nvariants - 1) in
        List.iter
          (fun p ->
            if p.nullable && List.mem p.pname all_bp && !problem = None then
              problem :=
                Some
                  (Printf.sprintf
                     "parameter :%s? is inside an optional block, which already makes it \
                      optional; drop the ?"
                     p.pname))
          full;
        match !problem with
        | Some m -> err file line "%s" m
        | None ->
            Ok
              ( fst variants.(nvariants - 1),
                snd variants.(nvariants - 1),
                {
                  nblocks;
                  variant_sqls = Array.map fst variants;
                  variant_nparams = Array.map (fun (_, ps) -> List.length ps) variants;
                  block_params;
                } ))
  end

(* ---------- headers ---------- *)

let strip s = String.trim s

(* "-- name: GetUser :one" -> Some ("GetUser", "one") *)
let parse_header line =
  let s = strip line in
  let prefix = "--" in
  if String.length s < 2 || String.sub s 0 2 <> prefix then None
  else
    let rest = strip (String.sub s 2 (String.length s - 2)) in
    let tag = "name:" in
    let tl = String.length tag in
    if String.length rest < tl || String.lowercase_ascii (String.sub rest 0 tl) <> tag
    then None
    else
      let rest = strip (String.sub rest tl (String.length rest - tl)) in
      match String.split_on_char ' ' rest |> List.filter (fun x -> x <> "") with
      | [ name; card ] when String.length card > 0 && card.[0] = ':' ->
          Some (name, String.sub card 1 (String.length card - 1))
      | _ -> None

let is_comment line =
  let s = strip line in
  String.length s >= 2 && String.sub s 0 2 = "--"

let cardinality_of_string = function
  | "one" -> Some One
  | "one!" -> Some One_strict
  | "many" -> Some Many
  | "exec" -> Some Exec
  | _ -> None

(* ---------- driver ---------- *)

open Gen_util

let strip_trailing_semicolon s =
  let s = String.trim s in
  let n = String.length s in
  if n > 0 && s.[n - 1] = ';' then String.trim (String.sub s 0 (n - 1)) else s

let of_string ~file contents =
  let lines = String.split_on_char '\n' contents in
  (* group into (header_line_no, name, card, body_lines) *)
  let groups = ref [] in
  let cur = ref None in
  List.iteri
    (fun idx line ->
      let lineno = idx + 1 in
      match parse_header line with
      | Some (name, card) ->
          (match !cur with Some g -> groups := g :: !groups | None -> ());
          cur := Some (lineno, name, card, ref [])
      | None -> (
          match !cur with
          | Some (_, _, _, body) -> body := line :: !body
          | None -> () (* preamble before the first header: ignored *)))
    lines;
  (match !cur with Some g -> groups := g :: !groups | None -> ());
  let groups = List.rev !groups in
  let rec build acc = function
    | [] -> Ok (List.rev acc)
    | (line, name, card, body) :: rest ->
        let* cardinality =
          match cardinality_of_string card with
          | Some c -> Ok c
          | None ->
              err file line "unknown cardinality %S (expected :one, :many or :exec)" card
        in
        let body = List.rev !body in
        (* leading comment lines directly under the header are the docstring *)
        let rec split_doc doc = function
          | l :: tl when is_comment l ->
              let s = strip l in
              split_doc (strip (String.sub s 2 (String.length s - 2)) :: doc) tl
          | l :: tl when strip l = "" && doc <> [] -> split_doc doc tl
          | rest -> (List.rev doc, rest)
        in
        let doc, sql_lines = split_doc [] body in
        let raw_sql = strip_trailing_semicolon (String.concat "\n" sql_lines) in
        let* () =
          if raw_sql = "" then err file line "query %S has no SQL" name else Ok ()
        in
        let* segs = split_blocks ~file ~line raw_sql in
        let has_blocks = List.exists (function `Block _ -> true | _ -> false) segs in
        let* sql, params, dynamic =
          if not has_blocks then
            let* sql, params = rewrite_params ~file ~line raw_sql in
            Ok (sql, params, None)
          else
            let* sql, params, dyn = build_dynamic ~file ~line segs in
            Ok (sql, params, Some dyn)
        in
        build
          ({
             name;
             module_name = to_module_name name;
             cardinality;
             doc;
             sql;
             params;
             dynamic;
             file;
             line;
           }
          :: acc)
          rest
  in
  build [] groups

let of_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let contents = really_input_string ic len in
  close_in ic;
  of_string ~file contents

let string_of_cardinality = function
  | One -> "one"
  | One_strict -> "one!"
  | Many -> "many"
  | Exec -> "exec"
