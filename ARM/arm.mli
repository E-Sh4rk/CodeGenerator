
type sign = int

type register = int

type conditional = EQ | NE | CS | HS | CC | LO | MI | PL 
                 | VS | VC | HI | LS | GE | LT | GT | LE | AL
type ldr_str_type = B | SB | H | SH | W
type addressing_type = Offset | PreIndexed | PostIndexed

type scale_type = LSL of int | LSR of int | ASR of int | ROR of int | RRX
type operand = Immediate of int32 | Register of register | ScaledRegister of register * scale_type
type register_offset = OImmediate of register * sign * int32 | ORegister of register * sign * register | OScaledRegister of register * sign * register * scale_type

type arm =
  | LDR of { typ: ldr_str_type ; cond: conditional ; rd: register ; ro: register_offset * addressing_type }
  | STR of { typ: ldr_str_type ; cond: conditional ; rd: register ; ro: register_offset * addressing_type }

  | MOV of { s:bool ; cond: conditional ; rd: register ; rs: operand }
  | MVN of { s:bool ; cond: conditional ; rd: register ; rs: operand }

  | ADC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }
  | SBC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }
  | BIC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }

exception Invalid

val sign_plus : sign
val sign_minus : sign

val a1 : register
val a2 : register
val a3 : register
val a4 : register
val v1 : register
val v2 : register
val v3 : register
val v4 : register
val v5 : register
val v6 : register
val v7 : register
val v8 : register
val sb : register (* static base *)
val sl : register (* stack limit/stack chunk handle *)
val fp : register (* frame pointer *)
val ip : register (* scratch register *)
val sp : register (* lower end of the current stack frame *)
val lr : register (* link register/scratch register *)
val pc : register (* program counter *)

val arm_to_binary : arm -> int32 list
val reverse_endianness  : int32 -> int32
