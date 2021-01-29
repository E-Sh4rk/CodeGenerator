
type offset =
  | OImmediate of Arm.sign * int32
  | ORegister of Arm.sign * string
  | ONone

type args =
  | Register of string
  | Immediate of int32
  | Offset of string (* register *) * offset * bool (* pre-indexed *)

type command =
  Lexing.position * string * args list

type ast = command list

val int32_of_str : string -> int32
val to_arm : ast -> (Arm.arm list) option
