
exception BoxFittingError of string

type fillers =
  { nop_code:int list ; nop_code_alt:int list; fillers:int list array }
let default_fillers () = {
  nop_code =
    if !Settings.game = Ruby || !Settings.game = Sapphire then
      [0x00 ; 0x00 ; 0x00 ; 0x00] (* 00000000 : andeq r0, r0, r0 *)
    else if !Settings.game = FireRed || !Settings.game = Settings.LeafGreen then
      [0x00 ; 0x00 ; 0x00 ; 0x00] (* 00000000 : andeq r0, r0, r0 *)
    else
      [0x00 ; 0x00 ; 0x00 ; 0x00] (* 00000000 : andeq r0, r0, r0 *)
    ;
  nop_code_alt =
    if !Settings.game = Ruby || !Settings.game = Sapphire then
      [0x00 ; 0x00 ; 0x00 ; 0xB0] (* B0000000 : andlt r0, r0, r0 *)
    else if !Settings.game = FireRed || !Settings.game = Settings.LeafGreen then
      [0x00 ; 0x00 ; 0x00 ; 0xB0] (* B0000000 : andlt r0, r0, r0 *)
    else
      [0x00 ; 0x00 ; 0x00 ; 0xB0] (* B0000000 : andlt r0, r0, r0 *)
    ;
  fillers = if !Settings.game = Ruby || !Settings.game = Sapphire
    then [|
      [0xFF ; 0x00 ; 0x00 ; 0xB0](* B00000FF *) ;
      [0x00 ; 0xFF ; 0x00 ; 0xB0](* B000FF00 *) ;
      [0x00 ; 0x00 ; 0xFF ; 0xB0](* B0FF0000 *) ;
      [0x00 ; 0x00 ; 0x00 ; 0xFF](* FF000000 *) ;
    |]
    else if !Settings.game = FireRed || !Settings.game = Settings.LeafGreen
    then [|
      [0xFF ; 0x00 ; 0x00 ; 0x00](* 000000FF *) ;
      [0x00 ; 0xFF ; 0x00 ; 0x00](* 0000FF00 *) ;
      [0x00 ; 0x00 ; 0xFF ; 0x00](* 00FF0000 *) ;
      [0xFF ; 0xFF ; 0xFF ; 0xFF](* FFFFFFFF *) ;
    |]
    else [|
      [0xFF ; 0x00 ; 0x00 ; 0x00](* 000000FF *) ;
      [0x00 ; 0xFF ; 0x00 ; 0x00](* 0000FF00 *) ;
      [0x00 ; 0x00 ; 0xFF ; 0x00](* 00FF0000 *) ;
      [0x00 ; 0x00 ; 0x00 ; 0xFF](* FF000000 *) ;
    |]
  }

let padding = [0x00 ; 0x00 ; 0x00 ; 0x00]
let m = List.length padding
let name_size = 8
let nb_boxes = 14
let eof = Name.eof

let no_eof codes =
  List.for_all (fun c -> c <> eof) codes

let only_eof codes =
  List.for_all (fun c -> c = eof) codes

let usable_eof_index codes =
  let next_box_content = codes |> List.drop_while (fun c -> Int.equal eof c |> not)
    |> List.drop_while (fun c -> Int.equal eof c) in
  (List.length codes) - 1 - (List.length next_box_content)

let first_non_eof_index codes =
  List.find_index (fun c -> Int.equal eof c |> not) codes
  |> Option.value ~default:(List.length codes)

let pad fillers pos =
  let pos = pos mod (name_size+1) in
  if pos + m <= name_size
  then padding
  else fillers.fillers.(name_size-pos)

let rec pad_nb fillers pos nb =
  if nb < 0
  then raise (BoxFittingError "Cannot pad the required amount. Is starting position valid?")
  else if nb = 0 then []
  else
    let code = pad fillers pos in
    code@(pad_nb fillers (pos + m) (nb - m))

let pack b = List.map (fun i -> (i, b))

let nop_code_at_pos fillers pos =
  if pos + m <= name_size then padding
  else fillers.fillers.(name_size-pos)

let fit_code_at_pos ?(next=Some []) fillers pos codes =
  let next = match next with Some next when only_eof next -> None | _ -> next in
  let rec aux rem_tries pos =
    if rem_tries <= 0 then raise (BoxFittingError "Box fitting algorithm failed.") ;
    let pos, n = pos mod (name_size+1), List.length codes in
    let is_ok_here =
      if no_eof codes
      then pos + n <= name_size
      else begin
        let i = usable_eof_index codes in
        (pos+i = name_size) || (* Already covers the EOF *)
        i=(n-1) && (pos+n <= name_size) && (
          pos+n+(pos+n |> nop_code_at_pos fillers |> first_non_eof_index)-1 = name_size
          || (* Can be followed by filler code *)
          match next with
          | Some next -> (* Can be followed by next code *)
            pos+n+(first_non_eof_index next)-1 = name_size
          | None -> true (* EOF will be covered because no more data *)
        )
      end
    in
    if is_ok_here then pack true codes
    else begin
      let nop_code = nop_code_at_pos fillers pos in
      (pack false nop_code)@(aux (rem_tries-1) (pos + m))
    end
  in
  aux (name_size+1) pos

let add_codes_after ?(final=false) fillers res codes =
  let rec aux acc codes =
    match codes with
    | [] -> acc
    | [codes] ->
      let next = if final then None else Some padding in
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
  Format.printf "Data:@." ;
  raw |> List.iter (fun x -> Format.printf "%02X " x) ;
  Format.printf "@." ;
  let rec split finished current codes i =
    match codes with
    | [] when i = 0 -> finished
    | []            ->
      let current =
        let n = List.length current in
        if fill_last && n = i (* If current box does not end by 0xFF *)
        then (List.init (name_size-n) (fun _ -> Name.space))@current
        else current
      in
      current::finished
    | c::codes when i = name_size && c = eof -> split (current::finished) [] codes 0
    | _::_     when i = name_size            -> raise (BoxFittingError
      "EOF expected, non-EOF character found. Please check the fillers.") ;
    | c::codes when c = eof -> split finished current codes (i+1)
    | c::codes              ->
      if List.length current <> i then raise (BoxFittingError
        "Non-EOF character found after an EOF in a box name. Please check the fillers.") ;
      split finished (c::current) codes (i+1)
  in
  split [] [] raw 0 |>
  List.map List.rev |>
  List.rev

let regroup_by n data =
  let rec aux acc k data =
    match data with
    | [] ->
      begin match acc with
      | _::acc when k > 0 -> acc
      | acc -> acc
      end
    | d::data ->
      if k = 0 then aux ([d]::acc) (n-1) data
      else begin
        match acc with
        | a::acc -> aux ((d::a)::acc) (k-1) data
        | _ -> assert false
      end
  in
  aux [] 0 data |> List.rev |> List.map List.rev

let unpack cmds =
  cmds |> List.map (fun elts ->
    let (cmd, bs) = List.split elts in
    assert (List.for_all (fun b -> b) bs || List.for_all (fun b -> not b) bs) ;
    (cmd, List.hd bs)
  )

let fit_codes_into_boxes ?(fill_last=true) ?(fillers) ?(start=0) ?(exit=None) codes =
  let fillers = Option.value ~default:(default_fillers ()) fillers in
  (* Main code *)
  let paddings = pad_nb fillers 0 start |> pack false in
  let res =
    add_codes_after ~final:(exit = None) fillers paddings codes in
  (* Add exit code *)
  let res =
    match exit with
    | None -> res
    | Some exit ->
      let i = List.length res in
      let (j,ecode) = Exit.get_preferred_raw exit i in
      let paddings = pad_nb fillers i (j-i) |> pack false in
      let res = res@paddings in
      add_codes_after ~final:true fillers res ecode
  in
  (* Split by box *)
  let res, unformatted = List.map fst res, (res  |> regroup_by 4 |> unpack) in
  let unformatted = unformatted |> List.map (fun (cmd, b) -> (Name.command_for_codes cmd, b)) in
  let res = split_raw_into_boxes ~fill_last res in
  (* If a box has a padding... *)
  let res = res |> List.mapi (fun i lst ->
    let pos = modulo (-i*(name_size+1)) m in
    let rec replace_if_padding first pos lst =
      if (pos+m) > name_size then lst else
      let lst = replace_if_padding false (pos+m) lst in
      if List.drop pos lst |> List.take m |> List.equal Int.equal padding then
        let rec try_replacements lst codes =
          match codes with
          | [] -> lst
          | code::codes ->
            let lst' = List.concat [
                List.take pos lst ;
                code ;
                List.drop (pos + m) lst
              ] |> List.rev |> List.drop_while (Int.equal eof) |> List.rev in
            if not (no_eof lst') then try_replacements lst codes
            else if first && Name.is_full_of_spaces lst' then try_replacements lst codes
            else lst'
        in
        try_replacements lst [fillers.nop_code ; fillers.nop_code_alt]
      else lst
    in
    replace_if_padding true pos lst
  ) in
  (res, unformatted)

let fit_codes_into_hex_boxes ?(exit=None) codes =
  (* Add exit code *)
  let codes =
    match exit with
    | None -> codes
    | Some exit ->
      let (_,ecode) = Exit.get_preferred_raw exit 0 in
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
