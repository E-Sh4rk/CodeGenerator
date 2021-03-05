
exception InvalidContent of string

type parsed_content =
  Preprocess.headers * (Arm.arm * Optimizer.optimization_setting) list

val from_lexbuf : headers:bool -> Lexing.lexbuf -> parsed_content
val from_str : headers:bool -> string -> parsed_content
val from_filename : headers:bool -> string -> parsed_content
