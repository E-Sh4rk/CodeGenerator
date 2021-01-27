open Int32
open Charset

exception Unwritable

let int8 = 0b11111111
let mask8 = int8 |> of_int

let int32_to_int v = match unsigned_to_int v with None -> assert false | Some i -> i

let writable_char_for_code i =
  if is_code_available i then char_at i else raise Unwritable

let codes_from_command v =
  let v1 = logand mask8 v in
  let v = shift_right_logical v 8 in
  let v2 = logand mask8 v in
  let v = shift_right_logical v 8 in
  let v3 = logand mask8 v in
  let v = shift_right_logical v 8 in
  let v4 = logand mask8 v in
  [ int32_to_int v1 ; int32_to_int v2 ; int32_to_int v3 ; int32_to_int v4 ]

let codes_to_chars c =
  List.map writable_char_for_code c

let chars_for_command v =
  codes_from_command v |> codes_to_chars

let pp_chars fmt lst =
  lst |> List.iter (fun str ->
    Format.fprintf fmt "%s " str
  )
