
module StrMap = Map.Make(String)

type t = Parse.parsed_content * (Parse.parsed_content StrMap.t)

let rec add_files lexbuf acc =
  if lexbuf.Lexing.lex_eof_reached then acc
  else (
    let (headers, arm) = Parse.from_lexbuf ~headers:true lexbuf in
    let fn =
      match Preprocess.get_param headers "filename" with
      | HNone -> failwith "Please specify the 'filename' header everywhere."
      | HString fn -> fn
      | _ -> failwith "Invalid headers."
    in
    StrMap.add fn (headers, arm) acc |> add_files lexbuf
  )

let parse lexbuf =
  lexbuf.Lexing.lex_eof_reached <- false ;
  let main_file = Parse.from_lexbuf ~headers:true lexbuf in
  let files = add_files lexbuf StrMap.empty in
  (main_file, files)

let from_filename filename =
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let res = parse lexbuf in
  close_in channel ; res

let from_str str =
  Lexing.from_string str |> parse

let main_file (mf, _) = mf

let get_file name (_, files) =
  StrMap.find name files
