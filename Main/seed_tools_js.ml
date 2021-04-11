open Seed_tools_common
open Js_of_ocaml

module Html = Dom_html

let cont = ref NoCont
let buffer = Buffer.create 1000
let fmt = Format.formatter_of_buffer buffer

let update_output () =
  let content = Buffer.contents buffer in
  let output = Option.get
      (Html.getElementById_coerce "output" Html.CoerceTo.textarea)
  in
  output##.value := Js.string content

let execute_and_print_continuation c =
  let c =
    try (
      let c = c fmt in
      Format.pp_print_flush fmt () ;
      c
    ) with e -> (Buffer.add_string buffer (Printexc.to_string e) ; NoCont)
  in
  update_output () ;
  c

let is_blank_str s =  
  let rec empty i =
    if i < 0 then true
    else
      let c = String.get s i in
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then empty (i-1)
      else false
  in
  empty ((String.length s)-1)

let get_input () =
  let input =
    Option.get
      (Html.getElementById_coerce "input" Html.CoerceTo.input)
  in
  let res = Js.to_string input##.value in
  input##.value := Js.string "" ;
  res

let enter _ =
  let input = get_input () in
  if is_blank_str input |> not
  then
    begin match !cont with
    | NoCont -> ()
    | Cont c ->
      Buffer.add_string buffer input ;
      Buffer.add_char buffer '\n' ;
      update_output () ;
      cont := execute_and_print_continuation (fun fmt -> c fmt input) ;
      if !cont = NoCont then (
        Buffer.add_string buffer "\n" ;
        update_output () ;
        cont := execute_and_print_continuation main
      )
    end ;
  Js._true

let init _ =
  let enter_button =
    Option.get
      (Html.getElementById_coerce "enter" Html.CoerceTo.button)
  in
  Html.addEventListener enter_button
    (Html.Event.make "click") (Html.handler enter) (Js.bool false)
  |> ignore ;
  cont := execute_and_print_continuation main ;
  Js._false

let _ = Html.addEventListener Html.window
  (Html.Event.make "load") (Html.handler init) (Js.bool false)
  |> ignore
