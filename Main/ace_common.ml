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
  code

let main fmt (headers, program) exit =
  let onlyraw =
    match Parser_ast.get_header headers "onlyraw" with
    | HNone -> false
    | HBool b -> b
    | _ -> failwith "Invalid headers."
  in
  if onlyraw && exit <> None
  then failwith "Only-raw mode does not support exit codes." ;
  let start =
    match Parser_ast.get_header headers "start" with
    | HNone -> 0
    | HInt i -> Name.int32_to_int i
    | _ -> failwith "Invalid headers."
  in
  let fillers =
    Array.init 4 (fun n ->
      let header_name = Format.sprintf "filler%n" (n+1) in
      match Parser_ast.get_header headers header_name with
      | HNone -> Boxes.nop_sequences.(n)
      | HInt i ->
        let codes = Name.codes_for_command i in
        if List.nth codes n <> Name.eof then failwith "Invalid filler." ;
        codes
      | _ -> failwith "Invalid headers."
    )
  in
  let res = program |> 
    Optimizer.fix_arm |>
    List.map (treat_command fmt) in
  if onlyraw
  then begin
    let start = List.init start (fun _ -> 0) in
    let res = List.concat (start::res) in
    Format.fprintf fmt "@.Raw data (in hexadecimal):@." ;
    List.iter (Format.fprintf fmt "%02X @?") res ;
    Format.fprintf fmt "@."
  end else
    try
      let boxes_codes = Boxes.fit_codes_into_boxes ~fillers ~start ~exit res in
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
      end ;
      Format.fprintf fmt "Raw data (in hexadecimal):@." ;
      boxes_codes |> List.iter (Format.fprintf fmt "%a" Boxes.pp_box_raw) ;
      Format.fprintf fmt "@."
    with Exit.NoExitCode ->
      Format.fprintf fmt "Error: The exit code overlaps this code (too long?).@."