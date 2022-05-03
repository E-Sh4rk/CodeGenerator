open Arm

let treat_command fmt arm =
  let hexs = arm_to_binary arm in
  let codes = List.map Name.codes_for_command hexs in
  let code = Name.preferred_code codes in
  let hex = Name.command_for_codes code in
  let chars = Name.codes_to_chars code in
  Format.fprintf fmt "%a \t%a\t%a@."
    Name.pp_chars chars
    Arm_printer.pp_hex hex
    Arm_printer.pp_arm arm ;
  (code, (hex, arm))

let append_terminators lst =
  let i = (Boxes.name_size+1) - (List.length lst) in
  lst@(List.init i (fun _ -> Name.eof))

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

let compare_and_print_commands fmt data descr exit =
  let rec aux data descr is_exit i =
    match data, descr with
    | [], _ -> ()
    | d::data, (hex, arm)::descr when Int32.equal d hex ->
      Format.fprintf fmt "%a@." Arm_printer.pp_arm arm ;
      aux data descr false (i+4)
    | _, [] when not is_exit && exit <> None ->
      let exit = Option.get exit in
      Format.fprintf fmt "; ======== EXIT CODE ========@." ;
      aux data (Exit.get_preferred_descr exit i |> snd) true i
    | d::data, _ ->
      Format.fprintf fmt "%a \t\t\t; (filler)@." Arm_printer.pp_arm (Arm.Custom d) ;
      aux data descr false (i+4)
  in
  aux data descr false 0

let main fmt env (headers,headers2) parsed exit =
  let onlyraw =
    match Preprocess.get_param headers "onlyraw" with
    | HNone -> false
    | HBool b -> b
    | _ -> failwith "Invalid headers."
  in
  if onlyraw && exit <> None
  then failwith "Only-raw mode does not support exit codes." ;
  let start =
    match Preprocess.get_param headers "start" with
    | HNone -> 0
    | HInt i -> Utils.uint32_to_int i
    | _ -> failwith "Invalid headers."
  in
  let fillers =
    Array.init 4 (fun n ->
      let header_name = Format.sprintf "filler%n" (n+1) in
      match Preprocess.get_param headers header_name with
      | HNone -> Boxes.default_fillers.fillers.(n)
      | HInt i ->
        let codes = Name.codes_for_command i in
        if List.nth codes n <> Name.eof then failwith "Invalid filler." ;
        codes
      | _ -> failwith "Invalid headers."
    )
  in
  let nop_code =
    match Preprocess.get_param headers "filler0" with
    | HNone -> Boxes.default_fillers.nop_code
    | HInt i -> Name.codes_for_command i
    | _ -> failwith "Invalid headers."
  in
  let nop_code_alt =
    match Preprocess.get_param headers "filler0_alt" with
    | HNone -> Boxes.default_fillers.nop_code_alt
    | HInt i -> Name.codes_for_command i
    | _ -> failwith "Invalid headers."
  in
  let fill_last =
    match Preprocess.get_param headers "fill",
          Preprocess.get_param headers2 "fill" with
    | HNone, HNone -> true
    | HNone, HBool b | HBool b, HNone -> b
    | HBool b1, HBool b2 when b1=b2 -> b1
    | HBool _, HBool _ ->
      failwith "The 'fill' header has a different value in the main code and in the exit code."
    | _ -> failwith "Invalid headers."
  in
  let (res, descr) =
    Parse.parsed_ast_to_arm ~optimize:true env parsed |>
    List.map (treat_command fmt) |> List.split in
  if onlyraw
  then begin
    let start = List.init start (fun _ -> 0) in
    let res = List.concat (start::res) in
    Format.fprintf fmt "@.Raw data (in hexadecimal):@." ;
    List.iter (Format.fprintf fmt "%02X @?") res ;
    Format.fprintf fmt "@." ; None
  end else
    try
      let fillers = { Boxes.fillers; Boxes.nop_code; Boxes.nop_code_alt } in
      let boxes_codes =
        if !Settings.hex_box_mode
        then Boxes.fit_codes_into_hex_boxes ~exit res
        else Boxes.fit_codes_into_boxes ~fill_last ~fillers ~start ~exit res
      in
      Format.fprintf fmt "@.%a@." Boxes.pp_boxes_names boxes_codes ;
      let size = List.length boxes_codes in
      begin
        if size > Boxes.nb_boxes
        then
          Format.fprintf fmt "Warning: Not enough space... Need %n/%n boxes.@."
          size Boxes.nb_boxes
      end ;
      begin
        if List.exists (fun c -> Name.is_code_writable c |> not) boxes_codes
        then Format.fprintf fmt "Warning: Contains unwritable characters...@."
        else if List.exists Name.is_full_of_spaces boxes_codes
        then Format.fprintf fmt "Warning: A box name cannot be written (only contains spaces)...@."
      end ;
      Format.fprintf fmt "All commands (with exit code and fillers):@." ;
      let data = List.map append_terminators boxes_codes |> List.concat |>
                 regroup_by 4 |> List.map Name.command_for_codes in
      compare_and_print_commands fmt data descr exit ;
      Format.fprintf fmt "@." ;
      Format.fprintf fmt "Raw data (in hexadecimal):@." ;
      boxes_codes |> List.iter (Format.fprintf fmt "%a" Boxes.pp_box_raw) ;
      Format.fprintf fmt "@." ;
      Some (List.map (fun c -> Name.codes_to_chars c |> Utils.concat_strings) boxes_codes)
    with Exit.NoExitCode ->
      failwith "The exit code overlaps this code (too long?).@."
