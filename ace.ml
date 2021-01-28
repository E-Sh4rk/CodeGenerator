open Arm

let program =
  [
    SBC {s=false; cond=AL; rd=11; rn=pc; op2=Immediate(0x2940)} ;
    LDR {typ=H; cond=AL; rd=12; rn=OImmediate(11, 21); addr_typ=Offset} ;
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x2840)} ;
    SBC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xDF)} ;
    STR {typ=H; cond=AL; rd=12; rn=OImmediate(11, 21); addr_typ=Offset} ;
    MOV {s=true; cond=AL; rd=12; rs=Immediate(0xBC00)} ;
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x384)} ;
    STR {typ=H; cond=AL; rd=12; rn=OImmediate(11, 37); addr_typ=Offset} ;

    ADC {s=true; cond=AL; rd=12; rn=pc; op2=Immediate(0x34)} ;
    MVN {s=false; cond=AL; rd=11; rs=Immediate(0xE1)} ;
    BIC {s=false; cond=AL; rd=11; rn=11; op2=Immediate(0xED00000)} ;
    BIC {s=false; cond=AL; rd=11; rn=11; op2=Immediate(0x1000000E)} ;
    STR {typ=W; cond=AL; rd=11; rn=OImmediate(12, 0); addr_typ=PreIndexed} ;
    ADC {s=false; cond=AL; rd=12; rn=lr; op2=Immediate(0xDC0)} ; 
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xD30000)} ;
    BIC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xC00000)} ;
    BIC {s=false; cond=AL; rd=0; rn=12; op2=Immediate(0xD6)} ;
  ]

let rec print_first_valid hexs =
  match hexs with
  | [] -> Format.printf "Unwritable\t\t\t"
  | hex::hexs ->
     try (
      let chars = Name.chars_for_command hex in
      Format.printf "%a\t%a\t"
        Arm_printer.pp_hex hex
        Name.pp_chars chars
    ) with Name.Unwritable -> print_first_valid hexs

let treat_command arm =
  let hexs = arm_to_binary arm in
  print_first_valid hexs ;
  Format.printf "%a@." Arm_printer.pp_arm arm
  
(*let () =
  let arm = ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x2840)} in
  treat_command arm*)

let () =
  program |> List.iter treat_command
