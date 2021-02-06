
exception InvalidContent of string

type parsed_content = Parser_ast.headers * (Arm.arm * bool) list

val from_lexbuf : headers:bool -> Lexing.lexbuf -> parsed_content
val from_str : headers:bool -> string -> parsed_content
val from_filename : headers:bool -> string -> parsed_content
