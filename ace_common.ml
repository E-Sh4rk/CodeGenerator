open Arm

let treat_command fmt arm =
  let hexs = arm_to_binary arm in
  let res =
    try
      let codes = List.map Name.codes_for_command hexs in
      let code = Name.preferred_code codes in
      let hex = Name.command_for_codes code in
      (if Name.is_code_writable code then
        let chars = Name.codes_to_chars code in
        Format.fprintf fmt "%a \t%a\t"
          Name.pp_chars chars
          Arm_printer.pp_hex hex
      else
        Format.fprintf fmt "Unwritable\t%a\t" Arm_printer.pp_hex hex ;
      ) ;
      Some code
    with
    | Not_found ->
      Format.fprintf fmt "Unwritable\t%a\t" Arm_printer.pp_hex (List.hd hexs) ;
      None
  in
  Format.fprintf fmt "%a@." Arm_printer.pp_arm arm ;
  res

let main fmt (headers, program) exit =
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
  let res = program |> List.map (treat_command fmt) in
  if List.for_all (fun o -> o <> None) res
  then begin
    try
      let boxes_codes = res |>
        List.map (function None -> assert false | Some s -> s) |>
        Boxes.fit_codes_into_boxes ~fillers ~start ~exit
      in
      Format.fprintf fmt "@.%a@." Boxes.pp_boxes_names boxes_codes ;
      let size = List.length boxes_codes in
      if size > Boxes.nb_boxes
      then
        Format.fprintf fmt "Warning: Not enough space... Need %n/%n boxes.@."
        size Boxes.nb_boxes
    with Exit.NoExitCode ->
      Format.fprintf fmt "Error: The exit code overlaps this code (too long?).@."
  end else Format.fprintf fmt "@.Codes are unwritable. Exiting.@."
