
exception BoxFittingError of string

type fillers =
  { nop_code:int list ; nop_code_alt:int list; fillers:int list array }
let default_fillers = {
  nop_code = [0x00 ; 0x00 ; 0x00 ; 0x00] (* 00000000 : andeq r0, r0, r0 *);
  nop_code_alt = [0x00 ; 0x00 ; 0x00 ; 0xB0] (* B0000000 : andlt r0, r0, r0 *);
  fillers= [|
    [0xFF ; 0x00 ; 0x00 ; 0x00](* 000000FF *) ;
    [0x00 ; 0xFF ; 0x00 ; 0x00](* 0000FF00 *) ;
    [0x00 ; 0x00 ; 0xFF ; 0x00](* 00FF0000 *) ;
    [0x00 ; 0x00 ; 0x00 ; 0xFF](* FF000000 *) ;
  |]
  }

let name_size = 8
let nb_boxes = 14

let eof = Name.eof

let no_eof codes =
  List.for_all (fun c -> c <> eof) codes

let rec only_consecutive_eof codes =
  match codes with
  | [] -> true
  | c::codes when c <> eof -> only_consecutive_eof codes
  | _::c'::codes when c' = eof -> only_consecutive_eof (c'::codes)
  | _::codes -> no_eof codes

let last_eof_index codes =
  let n = List.length codes in
  let codes = List.rev codes in
  let rec aux acc codes =
    match codes with
    | [] -> assert false
    | c::_ when c = eof -> acc
    | _::codes -> aux (acc+1) codes
  in
  n - 1 - (aux 0 codes)

let first_non_eof_index codes =
  let rec aux acc codes =
    match codes with
    | c::codes when c = eof -> aux (acc+1) codes
    | _ -> acc
  in
  aux 0 codes

let pad fillers pos =
  let pos = pos mod (name_size+1) in
  let n = List.length fillers.nop_code in
  if pos + n <= name_size
  then fillers.nop_code
  else fillers.fillers.(name_size-pos)

let rec pad_nb fillers pos nb =
  if nb < 0
  then raise (BoxFittingError "Cannot pad the required amount. Is starting position valid?")
  else if nb = 0 then []
  else
    let code = pad fillers pos in
    let m = List.length code in
    code@(pad_nb fillers (pos + m) (nb - m))

let rec fit_code_at_pos ?(next=Some []) fillers pos codes =
  let pos = pos mod (name_size+1) in
  let n = List.length codes in
  let is_ok_here =
    if no_eof codes
    then pos + n <= name_size
    else if only_consecutive_eof codes
    then
      let i = last_eof_index codes in
      let j = match next with Some next -> first_non_eof_index next | None -> 0 in
      (pos+i = name_size) ||
      (i = n-1 && pos+i+1 = name_size) || (* Followed by filler code *)
      (i = n-1 && pos+i+j = name_size) || (* Followed by next code *)
      (next = None && i = n-1 && pos+i <= name_size) (* Nothing after *)
    else raise (BoxFittingError
    "Some codes cannot be positionned due to non-consecutive 0xFF bytes.")
  in
  if is_ok_here then codes
  else begin
    let m = List.length fillers.nop_code in
    let nop_code =
      if pos + m <= name_size then fillers.nop_code
      else fillers.fillers.(name_size-pos)
    in
    let m = List.length nop_code in
    nop_code@(fit_code_at_pos ~next fillers (pos + m) codes)
  end

let add_codes_after ?(final=false) fillers res codes =
  let rec aux acc codes =
    match codes with
    | [] -> acc
    | [codes] ->
      let next = if final then None else Some [] in
      acc@(fit_code_at_pos ~next fillers (List.length acc) codes)
    | c1::c2::codes ->
      let nc = fit_code_at_pos ~next:(Some c2) fillers (List.length acc) c1 in
      aux (acc@nc) (c2::codes)
  in
  aux res codes

let modulo x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y

let split_raw_into_boxes ?(fill_last=false) raw =
  let rec split finished current codes i =
    match codes with
    | [] ->
      if i <> 0
      then begin
        let current =
          let n = List.length current in
          if fill_last && n = i (* If current box does not end by 0xFF *)
          then (List.init (name_size-n) (fun _ -> Name.space))@current
          else current
        in
        current::finished
      end else finished
    | c::codes when i = name_size ->
      if c <> eof
      then raise (BoxFittingError
      "Result is inconsistent. Please check the fillers.") ;
      split (current::finished) [] codes 0
    | c::codes when c = eof ->
      split finished current codes (i+1)
    | c::codes ->
      if List.length current <> i
      then raise (BoxFittingError
      "Result is inconsistent. Please check the fillers.") ;
      split finished (c::current) codes (i+1)
  in
  split [] [] raw 0 |>
  List.map List.rev |>
  List.rev

let fit_codes_into_boxes ?(fill_last=true) ?(fillers=default_fillers) ?(start=0) ?(exit=None) codes =
  (* Main code *)
  let padding = pad_nb fillers 0 start in
  let res =
    add_codes_after ~final:(exit = None) fillers padding codes in
  (* Add exit code *)
  let res =
    match exit with
    | None -> res
    | Some exit ->
      let i = List.length res in
      let (j,ecode) = Exit.get_preferred exit i in
      let padding = pad_nb fillers i (j-i) in
      let res = res@padding in
      add_codes_after ~final:true fillers res ecode
  in
  (* Split by box *)
  let res = split_raw_into_boxes ~fill_last res in
  (* If a box is full of spaces... *)
  res |> List.mapi (fun i lst ->
    if Name.is_full_of_spaces lst
    then
      let m = List.length fillers.nop_code_alt in
      let pos = modulo (-i*(name_size+1)) m in
      let prefix = List.init pos (fun _ -> Name.space) in
      let suffix_len = (List.length lst)-pos-m in
      if suffix_len < 0 then lst
      else
        let suffix = List.init suffix_len (fun _ -> Name.space) in
        List.concat [prefix ; fillers.nop_code_alt ; suffix]
    else lst
  )

let fit_codes_into_hex_boxes ?(exit=None) codes =
  (* Add exit code *)
  let codes =
    match exit with
    | None -> codes
    | Some exit ->
      let (_,ecode) = Exit.get_preferred exit 0 in
      codes @ ecode
  in
  (* Fit into boxes *)
  codes |> List.map (fun code ->
    code |> List.map (fun i ->
      let hex1 = Format.sprintf "%X" (i mod 16) in
      let hex2 = Format.sprintf "%X" (i / 16) in
      [ Charset.encode_writable_char hex2 ; Charset.encode_writable_char hex1 ]
    ) |> List.flatten
  )

let pp_boxes_names fmt lst =
  let pp_box i codes =
    let chars = Name.codes_to_chars codes in
    Format.fprintf fmt "Box %2i: %a\t[%a]@." (i+1)
      Name.pp_chars chars Name.pp_chars_raw chars
  in
  List.iteri pp_box lst

let pp_box_raw fmt lst =
  let pad =
    List.init (name_size+1-(List.length lst)) (fun _ -> eof) in
  lst@pad |> List.iter (Format.fprintf fmt "%02X @?")
