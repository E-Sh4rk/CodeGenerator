
type definition = string * string option
type headers = definition list

type offset =
  | OImmediate of Arm.sign * int32
  | ORegister of Arm.sign * string

type args =
  | Register of string
  | Immediate of int32
  | Offset of string (* register *) * offset * Arm.addressing_type

type command =
  | ASM of Lexing.position * string * args list
  | BIN of Lexing.position * int32

type ast = command list

exception CommandError of Lexing.position

val int32_of_str : string -> int32
val to_arm : ast -> Arm.arm list

val get_header : headers -> string -> string option
