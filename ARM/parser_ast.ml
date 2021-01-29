
type offset =
  | OImmediate of Arm.sign * int32
  | ORegister of Arm.sign * string
  | ONone

type args =
  | Registry of string
  | Immediate of int32
  | Offset of string (* registry *) * offset * bool (* pre-indexed *)

type command =
  Lexing.position * string * args list

type ast = command list

let int32_of_str str = String.lowercase_ascii str |> Int32.of_string
