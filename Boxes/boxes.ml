
exception NotEnoughSpace

let nop_sequences =
  [|
    [0xFF ; 0x00 ; 0xAC ; 0xB2](* B2AC00FF *) ;
    [0x00 ; 0xFF ; 0xAC ; 0xB2](* B2ACFF00 *) ;
    [0x00 ; 0x00 ; 0xFF ; 0xBF](* BFFF0000 *) ;
    [0x00 ; 0x00 ; 0x00 ; 0xFF](* FF000000 *) ;
  |]

let nop_code = [0x00 ; 0x00 ; 0x00 ; 0x00] (* andeq r0, r0, r0 *)

let name_size = 8
let nb_boxes = 14
let eof = 0xFF

let fit_codes_into_boxes codes =
  (* Add fillers *)
  let rec aux pos codes =
    let pos = pos mod (name_size+1) in
    let n = List.length codes in
    if pos + n <= name_size
    then codes
    else if List.nth codes (name_size-pos) = eof
    then codes
    else
      let nop = nop_sequences.(name_size-pos) in
      let m = List.length nop in
      nop@(aux (pos + m) codes)
  in
  let res =
    List.fold_left (fun res codes ->
      res @ (aux (List.length res) codes)
    ) [] codes
  in
  (* Split in boxes *)
  let rec split finished current codes =
    match codes with
    | [] -> current::finished
    | c::codes when c=eof -> split (current::finished) [] codes
    | c::codes -> split finished (c::current) codes
  in
  let res =
    split [] [] res |>
    List.map List.rev |>
    List.rev
  in
  if List.length res > nb_boxes
  then raise NotEnoughSpace
  else res

let pp_boxes_names fmt lst =
  let pp_box i codes =
    let chars = Name.codes_to_chars codes in
    Format.fprintf fmt "Box %i:" (i+1) ;
    List.iter (fun str -> Format.fprintf fmt " %s" str) chars ;
    Format.fprintf fmt "@."
  in
  List.iteri pp_box lst
