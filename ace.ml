open Arm

let program =
  [
    SBC {s=false; cond=AL; rd=11; rn=pc; op2=Immediate(0x2940)} ;
    LDR {typ=H; cond=AL; rd=12; rn=OImmediate(11, 21); addr_typ=Offset} ;
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x2840)} ;
    SBC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xDF)} ;
    STR {typ=H; cond=AL; rd=12; rn=OImmediate(11, 21); addr_typ=Offset} ;
    MOV {s=true; cond=AL; rd=12; rs=Immediate(0xBC00)} ;
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x348)} ;
    STR {typ=H; cond=AL; rd=12; rn=OImmediate(11, 37); addr_typ=Offset} ;

    ADC {s=true; cond=AL; rd=12; rn=pc; op2=Immediate(0x34)} ;
    MVN {s=false; cond=AL; rd=11; rs=Immediate(0xE1)} ;
    BIC {s=false; cond=AL; rd=11; rn=11; op2=Immediate(0xED00000)} ;
    BIC {s=false; cond=AL; rd=11; rn=11; op2=Immediate(0x1000000E)} ;
    STR {typ=W; cond=AL; rd=11; rn=OImmediate(12, 0); addr_typ=PreIndexed} ;
    ADC {s=false; cond=AL; rd=12; rn=lr; op2=Immediate(0xDC0)} ; 
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xD30000)} ;
    BIC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xC00000)} ;
    ADC {s=false; cond=AL; rd=0; rn=12; op2=Immediate(0xD6)} ;
  ]

let rec first_valid hexs =
  match hexs with
  | [] -> Format.printf "Unwritable\t\t\t" ; None
  | hex::hexs ->
     try (
      let chars = Name.chars_for_command hex in
      Format.printf "%a\t%a\t"
        Arm_printer.pp_hex hex
        Name.pp_chars chars ;
      Some hex
    ) with Name.Unwritable -> first_valid hexs

let treat_command arm =
  let hexs = arm_to_binary arm in
  let res = first_valid hexs in
  Format.printf "%a@." Arm_printer.pp_arm arm ;
  res
  
let () =
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
