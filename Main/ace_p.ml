open Ace_common

let () =
  (*Printexc.record_backtrace true ;*)
  if Array.length Sys.argv > 1 then Settings.configure Sys.argv.(1) ""
  else if Array.length Sys.argv > 2 then Settings.configure Sys.argv.(1) Sys.argv.(2) ;
  let fmt = Format.std_formatter in
  let fs = Fs.from_filename "input_p.txt" in
  let (headers, program) = Fs.main_file fs in
  let env = Preprocess.env_from_headers fmt headers in
  let (exit, headers2) =
    match Preprocess.get_param headers "exit" with
    | HNone -> (None, [])
    | HString fn ->
      let (headers, ast) = Fs.get_file fn fs in
      let exit = Exit.load_from_parsed_file fmt env (headers, ast) in
      (Some exit, headers)
    | _ -> failwith "Invalid headers."
  in
  main fmt env (headers, headers2) program exit |> ignore
