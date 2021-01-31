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
  Printexc.record_backtrace true ;
  let program = Parse.from_filename ~headers:true "test.txt" in
  match program with
  | None -> Format.printf "@.No program to convert. Exiting.@."
  | Some (headers, program) ->
    let exit =
      match Parser_ast.get_header headers "exit" with
      | None -> None
      | Some fn -> Some (
        Filename.concat "Files/ExitCodes" fn |>
        Exit.load_from_dir)
    in
    let res = program |> List.map treat_command in
    if List.for_all (fun o -> o <> None) res
    then
      let boxes_codes = res |>
        List.map (function None -> assert false | Some s -> s) |>
        Boxes.fit_codes_into_boxes ~exit
      in
      Format.printf "@.%a@." Boxes.pp_boxes_names boxes_codes
    else Format.printf "@.Codes are unwritable. Exiting.@."
