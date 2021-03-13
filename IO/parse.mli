
exception InvalidContent of string

type parsed_content =
  Preprocess.headers * Parser_ast.ast

val from_lexbuf : headers:bool -> Lexing.lexbuf -> parsed_content
val from_str : headers:bool -> string -> parsed_content
val from_filename : headers:bool -> string -> parsed_content

val parsed_content_to_arm :
  Format.formatter -> optimize:bool -> parsed_content -> Arm.arm list
