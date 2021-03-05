open Ace_common

let () =
  (*Printexc.record_backtrace true ;*)
  let fs = Fs.from_filename "input_p.txt" in
  let (headers, program) = Fs.main_file fs in
  let exit =
    match Preprocess.get_param headers "exit" with
    | HNone -> None
    | HString fn ->
      Some (Fs.get_file fn fs |> Exit.load_from_parsed_file)
    | _ -> failwith "Invalid headers."
  in
  main Format.std_formatter (headers, program) exit
