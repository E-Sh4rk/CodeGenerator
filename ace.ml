open Arm

let treat_command arm =
  let hexs = arm_to_binary arm in
  let res =
    try
      let codes = List.map Name.codes_for_command hexs in
      let code = Name.preferred_code codes in
      let hex = Name.command_for_codes code in
      (if Name.is_code_writable code then
        let chars = Name.codes_to_chars code in
        Format.printf "%a \t%a\t"
          Name.pp_chars chars
          Arm_printer.pp_hex hex
      else
        Format.printf "Unwritable\t%a\t" Arm_printer.pp_hex hex ;
      ) ;
      Some code
    with
    | Not_found ->
      Format.printf "Unwritable\t%a\t" Arm_printer.pp_hex (List.hd hexs) ;
      None
  in
  Format.printf "%a@." Arm_printer.pp_arm arm ;
  res

let () =
  (*Printexc.record_backtrace true ;*)
  let program = Parse.from_filename ~headers:true "test.txt" in
  match program with
  | None -> Format.printf "@.No program to convert. Exiting.@."
  | Some (headers, program) ->
    let start =
      match Parser_ast.get_header headers "start" with
      | HNone -> 0
      | HInt i -> Name.int32_to_int i
      | _ -> failwith "Invalid headers."
    in
    let exit =
      match Parser_ast.get_header headers "exit" with
      | HNone -> None
      | HString fn -> Some (
        (Filename.concat "Files" fn)^".txt" |> Exit.load_from_file)
      | _ -> failwith "Invalid headers."
    in
    let res = program |> List.map treat_command in
    if List.for_all (fun o -> o <> None) res
    then begin
      try
        let boxes_codes = res |>
          List.map (function None -> assert false | Some s -> s) |>
          Boxes.fit_codes_into_boxes ~start ~exit
        in
        Format.printf "@.%a@." Boxes.pp_boxes_names boxes_codes ;
        let size = List.length boxes_codes in
        if size > Boxes.nb_boxes
        then
          Format.printf "Warning: Not enough space... Need %n/%n boxes.@."
          size Boxes.nb_boxes
      with Exit.NoExitCode ->
        Format.printf "Error: The exit code overlaps this code (too long?).@."
    end else Format.printf "@.Codes are unwritable. Exiting.@."
