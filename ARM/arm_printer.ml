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

let print_register_offset fmt (ro, addr_typ) =
  let str = if addr_typ = PreIndexed then "!" else "" in
  match ro, addr_typ with
  | OImmediate (r,i), PostIndexed -> Format.fprintf fmt "[%a], #%a"
    print_register r print_relative_int i
  | ORegister (r,s,ro), PostIndexed -> Format.fprintf fmt "[%a], %s%a"
    print_register r (sign_to_str s) print_register ro
  | OImmediate (r,i), _ -> Format.fprintf fmt "[%a, #%a]%s"
    print_register r print_relative_int i str
  | ORegister (r,s,ro), _ -> Format.fprintf fmt "[%a, %s%a]%s"
    print_register r (sign_to_str s) print_register ro str
  | OScaledRegister _, _ -> failwith "Not implemented"

let pp_arm fmt arm =
  match arm with
  | LDR {typ;cond;rd;rn;addr_typ} -> Format.fprintf fmt "LDR%s%s %a %a"
    (cond_to_str cond) (ldr_str_type_to_str typ)
    print_register rd print_register_offset (rn, addr_typ)
  | STR {typ;cond;rd;rn;addr_typ} -> Format.fprintf fmt "STR%s%s %a %a"
    (cond_to_str cond) (ldr_str_type_to_str typ)
    print_register rd print_register_offset (rn, addr_typ)
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

let int32_to_int i =
  match Int32.unsigned_to_int i with
  | None -> failwith "Only 64bits machines are supported"
  | Some i -> i

let pp_hex fmt i =
  Format.fprintf fmt "%08X" (int32_to_int i)
