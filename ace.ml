open Arm

let program =
  [
    SBC {s=false; cond=AL; rd=11; rn=pc; op2=Immediate(0x2940 |> Int32.of_int)} ;
    LDR {typ=H; cond=AL; rd=12; ro=OImmediate(11, sign_plus, 21 |> Int32.of_int), Offset} ;
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x2840 |> Int32.of_int)} ;
    SBC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xDF |> Int32.of_int)} ;
    STR {typ=H; cond=AL; rd=12; ro=OImmediate(11, sign_plus, 21 |> Int32.of_int), Offset} ;
    MOV {s=true; cond=AL; rd=12; rs=Immediate(0xBC00 |> Int32.of_int)} ;
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x348 |> Int32.of_int)} ;
    STR {typ=H; cond=AL; rd=12; ro=OImmediate(11, sign_plus, 37 |> Int32.of_int), Offset} ;

    ADC {s=true; cond=AL; rd=12; rn=pc; op2=Immediate(0x34 |> Int32.of_int)} ;
    MVN {s=false; cond=AL; rd=11; rs=Immediate(0xE1 |> Int32.of_int)} ;
    BIC {s=false; cond=AL; rd=11; rn=11; op2=Immediate(0xED00000 |> Int32.of_int)} ;
    BIC {s=false; cond=AL; rd=11; rn=11; op2=Immediate(0x1000000E |> Int32.of_int)} ;
    STR {typ=W; cond=AL; rd=11; ro=OImmediate(12, sign_plus, 0 |> Int32.of_int), PreIndexed} ;
    ADC {s=false; cond=AL; rd=12; rn=lr; op2=Immediate(0xDC0 |> Int32.of_int)} ; 
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xD30000 |> Int32.of_int)} ;
    BIC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xC00000 |> Int32.of_int)} ;
    ADC {s=false; cond=AL; rd=0; rn=12; op2=Immediate(0xD6 |> Int32.of_int)} ;
  ]

let rec first_valid hexs =
  match hexs with
  | [] -> Format.printf "Unwritable\t\t\t" ; None
  | hex::hexs ->
     try (
      let chars = Name.chars_for_command hex in
      Format.printf "%a\t%a\t"
        Name.pp_chars chars
        Arm_printer.pp_hex hex ;
      Some hex
    ) with Name.Unwritable -> first_valid hexs

let treat_command arm =
  let hexs = arm_to_binary arm in
  let res = first_valid hexs in
  Format.printf "%a@." Arm_printer.pp_arm arm ;
  res
  
let () =
  (*Printexc.record_backtrace true ;*)
  let _ = Exit.load_from_dir "Files/ExitCodes" in
  let program = Parse.from_filename "test.txt" in
  match program with
  | None -> Format.printf "@.No program to convert. Exiting.@."
  | Some program ->
    let res = program |> List.map treat_command in
    if List.for_all (function None -> false | Some _ -> true) res
    then (
      let boxes_codes = res |>
        List.map (function None -> assert false | Some c -> [c]) |>
        List.flatten |> List.map Name.codes_for_command |>
        Boxes.fit_codes_into_boxes
      in
      Format.printf "@.%a@." Boxes.pp_boxes_names boxes_codes
    ) else
      Format.printf "@.Not all commands are writable. Exiting.@."
