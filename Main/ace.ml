open Ace_common

let () =
  (*Printexc.record_backtrace true ;*)
  let fmt = Format.std_formatter in
  let (headers, program) = Parse.from_filename ~headers:true "input.txt" in
  let env = Preprocess.env_from_headers fmt headers in
  let exit =
    match Preprocess.get_param headers "exit" with
    | HNone -> None
    | HString fn -> Some (
      (Filename.concat "Files" fn)^".txt" |> Exit.load_from_file fmt env)
    | _ -> failwith "Invalid headers."
  in
  main fmt env (headers, program) exit
