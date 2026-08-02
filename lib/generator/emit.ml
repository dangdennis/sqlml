(** The emit phase: resolve, then render. See [Resolve] and [Render]. *)

open Gen_util

let generate ?(config = Config.empty) ~src (described : Describe.described list) =
  let* resolved = map_result (Resolve.resolve config) described in
  let* rows = Resolve.collect_rows resolved in
  let* enums = Resolve.collect_enums resolved in
  Ok (Render.source ~src ~enums ~rows resolved)
