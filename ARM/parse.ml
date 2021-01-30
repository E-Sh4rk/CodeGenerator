open Lexer
open Lexing

let stderr = Format.err_formatter 

let print_position fmt pos =
  Format.fprintf fmt "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let print_lexbuf_pos fmt lexbuf =
  print_position fmt lexbuf.lex_curr_p

let parse_with_error f lexbuf =
  try Some (f lexbuf) with
  | SyntaxError msg ->
    Format.fprintf stderr "%a: %s\n" print_lexbuf_pos lexbuf msg;
    None
  | Parser.Error ->
    Format.fprintf stderr "%a: parser error\n" print_lexbuf_pos lexbuf;
    None

let parse ~headers lexbuf =
  let headers =
    if headers
    then parse_with_error (Parser.headers Lexer.read) lexbuf
    else Some []
  in
  match headers with
  | Some headers ->
    begin match parse_with_error (Parser.ast Lexer.read) lexbuf with
    | Some ast ->
      begin
        try Some (headers, Parser_ast.to_arm ast)
        with Parser_ast.CommandError pos -> (
          Format.fprintf stderr "%a: command error\n" print_position pos ;
          None)
      end
    | None -> None
    end
  | None -> None

let from_filename ~headers filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let res = parse ~headers lexbuf in
  close_in channel ; res

let from_str ~headers str =
  Lexing.from_string str |> (parse ~headers)
