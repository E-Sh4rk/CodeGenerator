open Ace_common

let () =
  (*Printexc.record_backtrace true ;*)
  let (headers, program) = Parse.from_filename ~headers:true "input.txt" in
  let exit =
    match Parser_ast.get_header headers "exit" with
    | HNone -> None
    | HString fn -> Some (
      (Filename.concat "Files" fn)^".txt" |> Exit.load_from_file)
    | _ -> failwith "Invalid headers."
  in
  main Format.std_formatter (headers, program) exit
