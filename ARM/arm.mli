
type sign = int

type register = int

type conditional = EQ | NE | CS | HS | CC | LO | MI | PL 
                 | VS | VC | HI | LS | GE | LT | GT | LE | AL
type ldr_str_type = B | SB | H | SH | W

type scale_type = LSL of int | LSR of int | ASR of int | ROR of int | RRX
type operand = Immediate of int | Register of register | ScaledRegister of register * scale_type
type register_offset = OImmediate of register * int | ORegister of register * sign * register | OScaledRegister of register * sign * register * scale_type
(* NOTE: pre-indexed and post-indexed load/store are not supported for now *)

type arm =
  | LDR of { typ: ldr_str_type ; cond: conditional ; rd: register ; rn: register_offset }
  | STR of { typ: ldr_str_type ; cond: conditional ; rd: register ; rn: register_offset }

  | MOV of { s:bool ; cond: conditional ; rd: register ; rs: operand }
  | MVN of { s:bool ; cond: conditional ; rd: register ; rs: operand }

  | ADC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }
  | SBC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }

  | BIC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }

  | BX of { cond: conditional ; rm: register }

val sign_plus : sign
val sign_minus : sign
val pc : register
val sp : register
val arm_to_binary : arm -> int32
