open Arm

let () =
  let arm = ADC {s=false; cond=AL; rd=12; rn=12; op2=Immediate(0x2840)} in
  let hex = arm_to_binary arm in
  Format.printf "%a@." Arm_printer.pp_hex hex
