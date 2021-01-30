open Int32
open Charset

exception Unwritable

let int8 = 0b11111111
let mask8 = int8 |> of_int

let int32_to_int v = match unsigned_to_int v with None -> assert false | Some i -> i

let writable_char_for_code i =
  if is_code_available i then char_at i else raise Unwritable

let codes_for_command v =
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
  codes_for_command v |> codes_to_chars

let pp_chars fmt lst =
  lst |> List.iteri (fun i str ->
    Format.fprintf fmt "%s%s" (if i = 0 then "" else " ") str
  )

let pp_chars_raw fmt lst =
  lst |> List.iter (fun str ->
    let str = if str = "␣" then " " else str in
    Format.fprintf fmt "%s" str
  )

let is_code_writable hex =
  try (
    codes_to_chars hex |> ignore ; true
  ) with Unwritable -> false

let rec first_writable_code hexs =
  match hexs with
  | [] -> raise Unwritable
  | hex::hexs ->
    if is_code_writable hex
    then hex
    else first_writable_code hexs

let preferred_code hexs =
  try first_writable_code hexs
  with Unwritable -> List.hd hexs
