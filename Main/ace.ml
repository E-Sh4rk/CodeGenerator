open Ace_common

let () =
  (*Printexc.record_backtrace true ;*)
  if Array.length Sys.argv > 1 then Settings.configure Sys.argv.(1) ;
  let fmt = Format.std_formatter in
  let (headers, program) = Parse.from_filename ~headers:true "input.txt" in
  let env = Preprocess.env_from_headers fmt headers in
  let (exit, headers2) =
    match Preprocess.get_param headers "exit" with
    | HNone -> (None, [])
    | HString fn ->
      let (headers, exit) =
        (Filename.concat "Files" fn)^".txt" |>
        Exit.load_from_file fmt env in
      (Some exit, headers)
    | _ -> failwith "Invalid headers."
  in
  main fmt env (headers, headers2) program exit |> ignore
