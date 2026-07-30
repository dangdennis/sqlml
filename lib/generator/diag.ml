(** The one error type of the generator pipeline.

    Parse, describe, emit and the driver code all report failures as a [t], so every
    message a user sees carries the same [file:line: Query: message] shape, and improving
    diagnostics means editing one printer. *)

type t = {
  file : string option;
  line : int option;
  query : string option;
  message : string;
}

let v ?file ?line ?query message = { file; line; query; message }

(** [error ?file ?line ?query fmt] builds an [Error] result directly. *)
let error ?file ?line ?query fmt =
  Printf.ksprintf (fun message -> Error { file; line; query; message }) fmt

let to_string t =
  String.concat ""
    [
      (match (t.file, t.line) with
      | Some f, Some l -> Printf.sprintf "%s:%d: " f l
      | Some f, None -> f ^ ": "
      | None, _ -> "");
      (match t.query with Some q -> q ^ ": " | None -> "");
      t.message;
    ]
