
type sign = int

type register = int

type conditional = EQ | NE | CS | HS | CC | LO | MI | PL 
                 | VS | VC | HI | LS | GE | LT | GT | LE | AL
type ldr_str_type = B | SB | H | SH | W | T | BT
type addressing_type = Offset | PreIndexed | PostIndexed

type 'a scale_type = LSL of 'a | LSR of 'a | ASR of 'a | ROR of 'a | RRX
type reg_or_imm = Reg of register | Imm of int32

type operand = Immediate of int32 | Register of register | ScaledRegister of register * reg_or_imm scale_type
type register_offset = OImmediate of register * sign * int32 | ORegister of register * sign * register | OScaledRegister of register * sign * register * int32 scale_type
type data_proc_instr = ADC | SBC | BIC | AND (* for JP: *) | ADD | SUB | ORR | EOR
type mov_instr = MOV | MVN
type mem_instr = LDR | STR

type arm =
  | Custom of int32
  | Mem of { instr: mem_instr ; typ: ldr_str_type ; cond: conditional ; rd: register ; ro: register_offset * addressing_type }
  | Mov of { instr: mov_instr ; s:bool ; cond: conditional ; rd: register ; rs: operand }
  | DataProc of { instr: data_proc_instr ; s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }
  | Branch of { l:bool ; cond: conditional ; target: int32 }
  | BranchX of { l:bool ; cond: conditional ; rm: register }
  (* BLX_imm is not supported yet *)

exception InvalidCommand

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

val rotate_right : int32 -> int32
val rotate_left : int32 -> int32