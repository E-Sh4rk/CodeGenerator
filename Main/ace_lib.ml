open Ace_common
open Js_of_ocaml

module Html = Dom_html

let is_blank_str s =  
  let rec empty i =
    if i < 0 then true
    else
      let c = String.get s i in
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then empty (i-1)
      else false
  in
  empty ((String.length s)-1)

let build code exit_codes =
  let main_input = Js.to_string code in
  let secondary_input = Js.to_string exit_codes in
  let str = main_input^(
    if is_blank_str secondary_input
    then "" else "\n=====\n"^secondary_input
  ) in

  Optimizer.init () ;
  let fmt = Utils.dummy_fmt in
  begin try (
    let fs = Fs.from_str str in
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
    let res = main fmt env (headers, headers2) program exit in
    match res with
    | None -> Js.null
    | Some res -> List.map Js.string res |> Array.of_list |> Js.array |> Js.some
  ) with _ -> Js.null end

let _ =
  Js.export "aceGen"
    (object%js
       method build code exit_codes = build code exit_codes
     end)
