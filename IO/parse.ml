open Lexer
open Lexing

exception InvalidContent of string

type parsed_content =
  Preprocess.headers * Parser_ast.ast

let print_position fmt pos =
  Format.fprintf fmt "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let print_lexbuf_pos fmt lexbuf =
  print_position fmt lexbuf.lex_curr_p

let parse_with_error f lexbuf =
  try f lexbuf with
  | SyntaxError msg ->
    raise (InvalidContent
      (Format.asprintf "%a: %s\n" print_lexbuf_pos lexbuf msg))
  | Parser.Error ->
    raise (InvalidContent
      (Format.asprintf "%a: parser error\n" print_lexbuf_pos lexbuf))

let from_lexbuf ~headers lexbuf =
  let headers =
    if headers
    then parse_with_error (Parser.headers Lexer.read) lexbuf
    else []
  in
  let ast = parse_with_error (Parser.ast Lexer.read) lexbuf in
  (headers, ast)

let from_filename ~headers filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let res = from_lexbuf ~headers lexbuf in
  close_in channel ; res

let from_str ~headers str =
  Lexing.from_string str |> from_lexbuf ~headers

let parsed_content_to_arm ~optimize (headers, lst) =
  try
    let env = Preprocess.env_from_headers headers in
    Parser_ast.to_arm env lst |>
    if optimize then Optimizer.tweak_arm else Optimizer.do_not_tweak_arm
  with Parser_ast.CommandError pos ->
    raise (InvalidContent
      (Format.asprintf "%a: command error\n" print_position pos)
    )
