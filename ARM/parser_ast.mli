
type unprocessed_int32 = ConstInt32 of int32 | MetaExpr of Preprocess.meta_expr
type imm_or_reg = Reg of string | Imm of unprocessed_int32

type 'a shift_type =
  | LSL of 'a
  | LSR of 'a
  | ASR of 'a
  | ROR of 'a
  | RRX

type offset =
  | OImmediate of Arm.sign * unprocessed_int32
  | ORegister of Arm.sign * string
  | OShift of Arm.sign * string * unprocessed_int32 shift_type

type args =
  | Register of string
  | Immediate of unprocessed_int32
  | Shift of imm_or_reg shift_type
  | Offset of string (* register *) * offset * Arm.addressing_type

type command =
  | ASM of Lexing.position * string * args list * Optimizer.tweaking_settings
  | BIN of Lexing.position * unprocessed_int32

type ast = command list

exception CommandError of Lexing.position

val to_arm : Preprocess.env -> ast -> (Arm.arm * Optimizer.tweaking_settings) list
