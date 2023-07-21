open Ace_common
open Js_of_ocaml

module Html = Dom_html

let treat_input lang game str =
  Settings.configure lang game ;
  Optimizer.init () ;
  let buffer = Buffer.create 1000 in
  let fmt = Format.formatter_of_buffer buffer in
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
    main fmt env (headers, headers2) program exit |> ignore
  ) with e -> Buffer.add_string buffer (Printexc.to_string e) end ;
  Format.pp_print_flush fmt () ;
  Buffer.contents buffer

let is_blank_str s =  
  let rec empty i =
    if i < 0 then true
    else
      let c = String.get s i in
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then empty (i-1)
      else false
  in
  empty ((String.length s)-1)

let compute _ =
  let main_input =
    Option.get
      (Html.getElementById_coerce "main" Html.CoerceTo.textarea)
  in
  let secondary_input =
    Option.get
      (Html.getElementById_coerce "secondary" Html.CoerceTo.textarea)
  in
  let output =
    Option.get
      (Html.getElementById_coerce "output" Html.CoerceTo.textarea)
  in
  let lang =
    Option.get
      (Html.getElementById_coerce "lang" Html.CoerceTo.select)
  in
  let game =
    Option.get
      (Html.getElementById_coerce "game" Html.CoerceTo.select)
  in
  let main_input = Js.to_string main_input##.value in
  let secondary_input = Js.to_string secondary_input##.value in
  let input = main_input^(
    if is_blank_str secondary_input
    then "" else "\n=====\n"^secondary_input
  ) in
  let lang = Js.to_string lang##.value in
  let game = Js.to_string game##.value in
  let res = treat_input lang game input in
  output##.value := Js.string res;
  Js._true

let init _ =
  let compute_button =
    Option.get
      (Html.getElementById_coerce "compute" Html.CoerceTo.button)
  in
  Html.addEventListener compute_button
    (Html.Event.make "click") (Html.handler compute) (Js.bool false)
  |> ignore ;
  Js._false

let _ = Html.addEventListener Html.window
  (Html.Event.make "load") (Html.handler init) (Js.bool false)
  |> ignore
