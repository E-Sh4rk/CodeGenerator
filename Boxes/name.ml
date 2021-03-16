open Int32
open Charset
open Utils

let eof = 0xFF
let space = 0x00

let int8 = 0b11111111
let mask8 = int8 |> of_int

let codes_for_command v =
  let v1 = logand mask8 v in
  let v = shift_right_logical v 8 in
  let v2 = logand mask8 v in
  let v = shift_right_logical v 8 in
  let v3 = logand mask8 v in
  let v = shift_right_logical v 8 in
  let v4 = logand mask8 v in
  [ uint32_to_int v1 ; uint32_to_int v2 ; uint32_to_int v3 ; uint32_to_int v4 ]

let command_for_codes codes =
  match codes with
  | [c1;c2;c3;c4] ->
    let v1 = c1 |> of_int in
    let v2 = c2 |> of_int in
    let v3 = c3 |> of_int in
    let v4 = c4 |> of_int in
    let v = shift_left v4 8 in
    let v = logor v v3 in
    let v = shift_left v 8 in
    let v = logor v v2 in
    let v = shift_left v 8 in
    logor v v1
  | _ -> assert false

let codes_to_chars c =
  List.map writable_char_at c

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

let is_code_writable codes =
  List.for_all is_code_available codes

let rec first_code f codes =
  match codes with
  | [] -> raise Not_found
  | code::codes ->
    if f code then code
    else first_code f codes

let first_writable_code codes =
  first_code is_code_writable codes

let is_code_writable_or_one_eof code =
  (List.filter (fun c -> c <> eof) code |> is_code_writable) &&
  (List.fold_left (fun nb c -> if c = eof then nb+1 else nb) 0 code) <= 1

let preferred_code codes =
  try first_writable_code codes
  with Not_found -> begin
    try first_code is_code_writable_or_one_eof codes
    with Not_found -> List.hd codes
  end

let is_full_of_spaces codes =
  List.for_all (fun c -> c = space) codes
