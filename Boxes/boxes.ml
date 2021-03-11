
let default_fillers =
  [|
    [0xFF ; 0x00 ; 0x00 ; 0x00](* 000000FF *) ;
    [0x00 ; 0xFF ; 0x00 ; 0x00](* 0000FF00 *) ;
    [0x00 ; 0x00 ; 0xFF ; 0x00](* 00FF0000 *) ;
    [0x00 ; 0x00 ; 0x00 ; 0xFF](* FF000000 *) ;
  |]

let nop_code = [0x00 ; 0x00 ; 0x00 ; 0x00] (* 00000000 : andeq r0, r0, r0 *)
let nop_code2 = [0x00 ; 0x00 ; 0x00 ; 0xB0] (* B0000000 : andlt r0, r0, r0 *)

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

let pad fillers pos =
  let pos = pos mod (name_size+1) in
  let n = List.length nop_code in
  if pos + n <= name_size
  then nop_code
  else fillers.(name_size-pos)

let rec pad_nb fillers pos nb =
  if nb < 0 then failwith "Invalid starting position."
  else if nb = 0 then []
  else
    let code = pad fillers pos in
    let m = List.length code in
    code@(pad_nb fillers (pos + m) (nb - m))

let rec fit_code_at_pos fillers pos codes =
  let pos = pos mod (name_size+1) in
  let n = List.length codes in
  if pos + n <= name_size
  then begin
    if no_eof codes then codes
    else
      let m = List.length nop_code in
      nop_code@(fit_code_at_pos fillers (pos + m) codes)
  end else if eof_only_at_pos codes (name_size-pos)
  then codes
  else
    let nop = fillers.(name_size-pos) in
    let m = List.length nop in
    nop@(fit_code_at_pos fillers (pos + m) codes)

let add_codes_after fillers res codes =
  List.fold_left (fun res codes ->
    res @ (fit_code_at_pos fillers (List.length res) codes)
  ) res codes

let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

let fit_codes_into_boxes ?(fillers=default_fillers) ?(start=0) ?(exit=None) codes =
  (* Main code *)
  let padding = pad_nb fillers 0 start in
  let res = add_codes_after fillers padding codes in
  (* Add exit code *)
  let res =
    match exit with
    | None -> res
    | Some exit ->
      let i = List.length res in
      let (j,ecode) = Exit.get_preferred exit i in
      let padding = pad_nb fillers i (j-i) in
      let res = res@padding in
      add_codes_after fillers res ecode
  in
  (* Split in boxes *)
  let rec split finished current codes =
    match codes with
    | [] -> if current <> [] then current::finished else finished
    | c::codes when c=eof -> split (current::finished) [] codes
    | c::codes -> split finished (c::current) codes
  in
  let res =
    split [] [] res |>
    List.map List.rev |>
    List.rev
  in
  (* If a box is full of 0... *)
  res |> List.mapi (fun i lst ->
    if List.for_all (fun c -> c = 0) lst
    then
      let m = List.length nop_code2 in
      let pos = modulo (m - i*(name_size+1)) m in
      let prefix = List.init pos (fun _ -> 0) in
      let suffix = List.init (name_size-pos-m) (fun _ -> 0) in
      List.concat [prefix ; nop_code2 ; suffix]
    else lst
  )

let pp_boxes_names fmt lst =
  let pp_box i codes =
    let chars = Name.codes_to_chars codes in
    Format.fprintf fmt "Box %2i: %a\t[%a]@." (i+1)
      Name.pp_chars chars Name.pp_chars_raw chars
  in
  List.iteri pp_box lst

let pp_box_raw fmt lst =
  lst@[eof] |> List.iter (Format.fprintf fmt "%02X @?")
