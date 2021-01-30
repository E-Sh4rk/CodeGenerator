open Lexer
open Lexing

let stderr = Format.err_formatter 

let print_position fmt lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf fmt "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Some (Parser.ast Lexer.read lexbuf) with
  | SyntaxError msg ->
    Format.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    Format.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    None

let parse lexbuf =
  match parse_with_error lexbuf with
  | Some ast -> Parser_ast.to_arm ast
  | None -> None

let from_filename filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let res = parse lexbuf in
  close_in channel ; res

let from_str str =
  Lexing.from_string str |> parse
