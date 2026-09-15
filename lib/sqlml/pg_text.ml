(* Container grammars share escaping, but not NULL or delimiter semantics. *)
let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      if c = '"' || c = '\\' then Buffer.add_char b '\\';
      Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let token s pos ~stop =
  let n = String.length s and b = Buffer.create 16 in
  let quoted = ref false and escaped = ref false in
  if !pos < n && s.[!pos] = '"' then begin
    quoted := true;
    incr pos;
    let rec read () =
      if !pos >= n then invalid_arg "unterminated quoted container value";
      let c = s.[!pos] in
      incr pos;
      if c = '\\' then begin
        if !pos >= n then invalid_arg "unterminated escape";
        Buffer.add_char b s.[!pos];
        incr pos;
        read ()
      end
      else if c = '"' then
        begin if !pos < n && s.[!pos] = '"' then (
          Buffer.add_char b '"';
          incr pos;
          read ())
        end
      else (
        Buffer.add_char b c;
        read ())
    in
    read ()
  end
  else begin
    while !pos < n && not (stop s.[!pos]) do
      let c = s.[!pos] in
      incr pos;
      if c = '"' then invalid_arg "unexpected quote";
      if c = '\\' then begin
        escaped := true;
        if !pos >= n then invalid_arg "unterminated escape";
        Buffer.add_char b s.[!pos];
        incr pos
      end
      else Buffer.add_char b c
    done
  end;
  let v = Buffer.contents b in
  (v, !quoted || !escaped)

let context path f =
  try f () with Invalid_argument m | Failure m -> invalid_arg (path ^ ": " ^ m)
