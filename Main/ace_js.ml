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

let build lang game code exit_codes =
  let main_input = Js.to_string code in
  let secondary_input = Js.to_string exit_codes in
  let lang = Js.to_string lang in
  let game = Js.to_string game in
  Settings.configure lang game ;
  Optimizer.init () ;
  let str = main_input^(
    if is_blank_str secondary_input
    then "" else "\n=====\n"^secondary_input
  ) in
  let buffer = Buffer.create 1000 in
  let fmt = Format.formatter_of_buffer buffer in
  let res =
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
    ) with e -> Buffer.add_string buffer (Printexc.to_string e) ; Js.null end
  in
  Format.pp_print_flush fmt () ;
  let txt = Js.string (Buffer.contents buffer) in
  [| Js.Unsafe.inject res ; Js.Unsafe.inject txt |] |> Js.array

let _ =
  Js.export "aceGen"
    (object%js
       method build lang game code exit_codes = build lang game code exit_codes
     end)
