open Ace_common

let () =
  (*Printexc.record_backtrace true ;*)
  let fmt = Format.std_formatter in
  let fs = Fs.from_filename "input_p.txt" in
  let (headers, program) = Fs.main_file fs in
  let env = Preprocess.env_from_headers fmt headers in
  let exit =
    match Preprocess.get_param headers "exit" with
    | HNone -> None
    | HString fn ->
      Some (Fs.get_file fn fs |> Exit.load_from_parsed_file fmt env)
    | _ -> failwith "Invalid headers."
  in
  main fmt env (headers, program) exit
