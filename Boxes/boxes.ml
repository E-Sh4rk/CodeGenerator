
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

exception ReturnFalse

let eof = Name.eof

let eof_only_at_pos codes i =
  try begin
    List.iteri (fun j code ->
      if j=i && code <> eof then raise ReturnFalse
      else if j<>i && code = eof then raise ReturnFalse
    ) codes ; true
  end with ReturnFalse -> false

let no_eof codes =
  List.for_all (fun c -> c <> eof) codes

let fit_codes_into_boxes ?(exit=None) codes =
  (* Add fillers *)
  let rec aux pos codes =
    let pos = pos mod (name_size+1) in
    let n = List.length codes in
    if pos + n <= name_size
    then begin
      if no_eof codes then codes
      else
        let m = List.length nop_code in
        nop_code@(aux (pos + m) codes)
    end else if eof_only_at_pos codes (name_size-pos)
    then codes
    else
      let nop = nop_sequences.(name_size-pos) in
      let m = List.length nop in
      nop@(aux (pos + m) codes)
  in
  let add_codes_after res codes =
    List.fold_left (fun res codes ->
      res @ (aux (List.length res) codes)
    ) res codes
  in
  let res = add_codes_after [] codes in
  (* Add exit code *)
  let pad pos =
    let pos = pos mod (name_size+1) in
    let n = List.length nop_code in
    if pos + n <= name_size
    then nop_code
    else nop_sequences.(name_size-pos)
  in
  let rec pad_nb pos nb =
    if nb < 0 then assert false
    else if nb = 0 then []
    else
      let code = pad pos in
      let m = List.length code in
      code@(pad_nb (pos + m) (nb - m))
  in
  let res =
    match exit with
    | None -> res
    | Some exit ->
      let i = List.length res in
      let (j,ecode) = Exit.get_preferred exit i in
      let padding = pad_nb i (j-i) in
      let res = res@padding in
      add_codes_after res ecode
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
    Format.fprintf fmt "Box %2i: %a\t[%a]@." (i+1)
      Name.pp_chars chars Name.pp_chars_raw chars
  in
  List.iteri pp_box lst
