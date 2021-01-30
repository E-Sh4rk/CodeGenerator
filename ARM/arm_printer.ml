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
  | T -> "T" | BT -> "BT"

let sign_to_str sign =
  if sign = sign_minus then "-" else ""

let s_to_str s =
  if s then "S" else ""

let print_register fmt r =
  Format.fprintf fmt "r%i" r

let print_operand fmt op =
  match op with
  | Immediate i -> Format.fprintf fmt "#%#lx" i
  | Register r -> Format.fprintf fmt "%a" print_register r
  | ScaledRegister _ -> failwith "Not implemented"

let print_immediate_offset fmt (s, i) =
  Format.fprintf fmt "#%s%#lx" (sign_to_str s) i

let print_register_offset fmt (ro, addr_typ) =
  let str = if addr_typ = PreIndexed then "!" else "" in
  match ro, addr_typ with
  | OImmediate (r,s,i), PostIndexed -> Format.fprintf fmt "[%a], %a"
    print_register r print_immediate_offset (s, i)
  | ORegister (r,s,ro), PostIndexed -> Format.fprintf fmt "[%a], %s%a"
    print_register r (sign_to_str s) print_register ro
  | OImmediate (r,s,i), _ -> Format.fprintf fmt "[%a, %a]%s"
    print_register r print_immediate_offset (s, i) str
  | ORegister (r,s,ro), _ -> Format.fprintf fmt "[%a, %s%a]%s"
    print_register r (sign_to_str s) print_register ro str
  | OScaledRegister _, _ -> failwith "Not implemented"

let pp_arm fmt arm =
  match arm with
  | LDR {typ;cond;rd;ro} -> Format.fprintf fmt "LDR%s%s %a, %a"
    (cond_to_str cond) (ldr_str_type_to_str typ)
    print_register rd print_register_offset ro
  | STR {typ;cond;rd;ro} -> Format.fprintf fmt "STR%s%s %a, %a"
    (cond_to_str cond) (ldr_str_type_to_str typ)
    print_register rd print_register_offset ro
  | MOV {s;cond;rd;rs}   -> Format.fprintf fmt "MOV%s%s %a, %a"
    (cond_to_str cond) (s_to_str s) print_register rd print_operand rs
  | MVN {s;cond;rd;rs}   -> Format.fprintf fmt "MVN%s%s %a, %a"
    (cond_to_str cond) (s_to_str s) print_register rd print_operand rs
  | ADC {s;cond;rd;rn;op2} -> Format.fprintf fmt "ADC%s%s %a, %a, %a"
    (cond_to_str cond) (s_to_str s)
    print_register rd print_register rn print_operand op2
  | SBC {s;cond;rd;rn;op2} -> Format.fprintf fmt "SBC%s%s %a, %a, %a"
    (cond_to_str cond) (s_to_str s)
    print_register rd print_register rn print_operand op2
  | BIC {s;cond;rd;rn;op2} -> Format.fprintf fmt "BIC%s%s %a, %a, %a"
    (cond_to_str cond) (s_to_str s)
    print_register rd print_register rn print_operand op2

let pp_hex fmt i =
  Format.fprintf fmt "%08lX" i
