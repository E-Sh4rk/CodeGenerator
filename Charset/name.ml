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
    let str = if str = spacing_char then " " else str in
    Format.fprintf fmt "%s" str
  )

let is_code_writable codes =
  List.for_all is_code_available codes

let score codes =
  let unwritable = codes |> List.mapi (fun i c -> (i,c))
    |> List.filter (fun (_,c) -> is_code_available c |> not)
  in
  if unwritable |> List.exists (fun (_,c) -> c <> eof)
  then Int.max_int
  else
    let eof_indexes = unwritable |> List.map fst in
    let aux (score,last_i) i =
      match last_i with
      | None -> (score+1, Some i)
      | Some j when i=j+1 -> (score+1, Some i)
      | Some _ -> (score+5, Some i)
    in
    List.fold_left aux (0,None) eof_indexes |> fst

let min f lst =
  let aux acc e =
    let score = f e in
    match acc with
    | None -> Some (score, e)
    | Some (min,_) when score < min -> Some (score, e)
    | Some (min,min_e) -> Some (min, min_e)
  in
  match List.fold_left aux None lst with
  | None -> raise Not_found
  | Some (_,e) -> e

let preferred_code codes = min score codes

let is_full_of_spaces codes =
  List.for_all (fun c -> c = space) codes
