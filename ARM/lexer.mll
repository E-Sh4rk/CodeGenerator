{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let eof_reached lexbuf =
  lexbuf.lex_eof_reached <- true
}

let ddigit = ['0'-'9']
let hdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let bdigit = ['0'-'1']
let odigit = ['0'-'7']

let dprefix = "0u" | "0U"
let hprefix = "0x" | "0X"
let bprefix = "0b" | "0B"
let oprefix = "0o" | "0O"

let dnumber = dprefix? ddigit+
let hnumber = hprefix hdigit+
let bnumber = bprefix bdigit+
let onumber = oprefix odigit+

let number = dnumber | hnumber | bnumber | onumber

let comment = "@" | "%%" | ";" | "//"

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

let file_separator = "=====" '='*

rule read = parse
  | file_separator { EOF }
  | "@@"      { HEADER }
  | "null"    { NULL }
  | "true"    { BOOL true }
  | "false"   { BOOL false }
  | comment   { read_comment lexbuf }
  | white     { read lexbuf }
  | newline   { next_line lexbuf ; EOL }
  | number    { NUMBER (Parser_ast.int32_of_str (Lexing.lexeme lexbuf)) }
  | id        { ID (Lexing.lexeme lexbuf) }
  | '"'       { read_string (Buffer.create 17) lexbuf }
  | '='       { EQUAL }
  | '#'       { HASH }
  | '['       { LEFT_BRACK }
  | ']'       { RIGHT_BRACK }
  | ','       { COMMA }
  | '!'       { EXCLAM_MARK }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | eof       { eof_reached lexbuf ; EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\' '\r' '\n']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | newline { raise (SyntaxError ("String cannot be multiline")) }
  | eof { raise (SyntaxError ("String is not terminated")) }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_comment = parse
  | newline { next_line lexbuf ; EOL }
  | eof { eof_reached lexbuf ; EOF }
  | _ { read_comment lexbuf }