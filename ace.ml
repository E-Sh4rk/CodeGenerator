open Arm

let program =
  [
    SBC {s=false; cond=AL; rd=11; rn=pc; op2=Immediate(0x2940)} ;
    LDR {typ=H; cond=AL; rd=12; rn=OImmediate(11, 21)} ;
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x2840)} ;
    SBC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xDF)} ;
    STR {typ=H; cond=AL; rd=12; rn=OImmediate(11, 21)} ;
    MOV {s=false; cond=AL; rd=12; rs=Immediate(0xBC00)} ;
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x384)} ;
    STR {typ=H; cond=AL; rd=12; rn=OImmediate(11, 37)} ;

    (* Multiple ways to assemble the expressions below... find the good one *)
    ADC {s=true; cond=AL; rd=12; rn=pc; op2=Immediate(0x34)} ;
    MVN {s=false; cond=AL; rd=11; rs=Immediate(0xE1)} ;
    BIC {s=false; cond=AL; rd=11; rn=11; op2=Immediate(0xED00000)} ;
    BIC {s=false; cond=AL; rd=11; rn=11; op2=Immediate(0x1000000E)} ;
    STR {typ=W; cond=AL; rd=11; rn=OImmediate(12, 0)} ;
    ADC {s=false; cond=AL; rd=12; rn=lr; op2=Immediate(0xDC0)} ; 
    ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xD30000)} ;
    BIC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0xC00000)} ;
    BIC {s=false; cond=AL; rd=0; rn=12; op2=Immediate(0xD6)} ;
  ]

let treat_command arm =
  let hex = arm_to_binary arm in
  Format.printf "%a\t%a@."
    Arm_printer.pp_hex hex
    Arm_printer.pp_arm arm ;
  try (
    let chars = Name.chars_for_command hex in
    Format.printf "%a@." Name.pp_chars chars
  ) with Name.Unwritable -> Format.printf "Unwritable@."

(*let () =
  let arm = ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x2840)} in
  treat_command arm*)

let () =
  program |> List.iter treat_command
