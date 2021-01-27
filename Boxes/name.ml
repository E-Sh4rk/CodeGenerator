open Int32
open Charset

exception Unwritable

let int8 = 0b11111111
let mask8 = int8 |> of_int

let char_for_int32 v =
  let i = match unsigned_to_int v with None -> assert false | Some i -> i in
  if is_code_available i then char_at i else raise Unwritable

let chars_for_command v =
  let v1 = logand mask8 v in
  let v = shift_right_logical v 8 in
  let v2 = logand mask8 v in
  let v = shift_right_logical v 8 in
  let v3 = logand mask8 v in
  let v = shift_right_logical v 8 in
  let v4 = logand mask8 v in
  [ char_for_int32 v1 ; char_for_int32 v2 ; char_for_int32 v3 ; char_for_int32 v4 ]

let pp_chars fmt lst =
  lst |> List.iter (fun str ->
    Format.fprintf fmt "%s " str
  )
