open Arm

let cond_to_str c =
  match c with
  | EQ -> "EQ" | NE -> "NE" | CS -> "CS" | HS -> "HS"
  | CC -> "CC" | LO -> "LO" | MI -> "MI" | PL -> "PL" 
  | VS -> "VS" | VC -> "VC" | HI -> "HI" | LS -> "LS"
  | GE -> "GE" | LT -> "LT" | GT -> "GT" | LE -> "LE"
  | AL -> ""

let ldr_str_type_to_str t =
  match t with
  | B -> "B" | SB -> "SB" | H -> "H" | SH -> "SH" | W -> ""

let sign_to_str sign =
  if sign = sign_minus then "-" else ""

let s_to_str s =
  if s then "S" else ""

let print_relative_int fmt i =
  if i >= 0
  then Format.fprintf fmt "%#x" i
  else Format.fprintf fmt "-%#x" (-i)

let print_register fmt r =
  Format.fprintf fmt "r%i" r

let print_operand fmt op =
  match op with
  | Immediate i -> Format.fprintf fmt "#%a" print_relative_int i
  | Register r -> Format.fprintf fmt "%a" print_register r
  | ScaledRegister _ -> failwith "Not implemented"

let print_register_offset fmt ro =
  match ro with
  | OImmediate (r,i) -> Format.fprintf fmt "[%a, #%a]"
    print_register r print_relative_int i
  | ORegister (r,s,ro) -> Format.fprintf fmt "[%a, %s%a]"
    print_register r (sign_to_str s) print_register ro
  | OScaledRegister _ -> failwith "Not implemented"

let pp_arm fmt arm =
  match arm with
  | LDR {typ;cond;rd;rn} -> Format.fprintf fmt "LDR%s%s %a %a"
    (cond_to_str cond) (ldr_str_type_to_str typ)
    print_register rd print_register_offset rn
  | STR {typ;cond;rd;rn} -> Format.fprintf fmt "STR%s%s %a %a"
    (cond_to_str cond) (ldr_str_type_to_str typ)
    print_register rd print_register_offset rn
  | MOV {s;cond;rd;rs}   -> Format.fprintf fmt "MOV%s%s %a %a"
    (cond_to_str cond) (s_to_str s) print_register rd print_operand rs
  | MVN {s;cond;rd;rs}   -> Format.fprintf fmt "MVN%s%s %a %a"
    (cond_to_str cond) (s_to_str s) print_register rd print_operand rs
  | ADC {s;cond;rd;rn;op2} -> Format.fprintf fmt "ADC%s%s %a %a %a"
    (cond_to_str cond) (s_to_str s)
    print_register rd print_register rn print_operand op2
  | SBC {s;cond;rd;rn;op2} -> Format.fprintf fmt "SBC%s%s %a %a %a"
    (cond_to_str cond) (s_to_str s)
    print_register rd print_register rn print_operand op2
  | BIC {s;cond;rd;rn;op2} -> Format.fprintf fmt "BIC%s%s %a %a %a"
    (cond_to_str cond) (s_to_str s)
    print_register rd print_register rn print_operand op2
  | BX {cond;rm} -> Format.fprintf fmt "BX%s %a"
    (cond_to_str cond) print_register rm

let int32_to_int i =
  match Int32.unsigned_to_int i with
  | None -> failwith "Only 64bits machines are supported"
  | Some i -> i

let pp_hex fmt i =
  Format.fprintf fmt "%08X" (int32_to_int i)
