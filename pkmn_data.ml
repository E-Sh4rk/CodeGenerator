open Pkmn_data_common

let rec continue_until_end fmt c =
  match c with
  | NoCont -> Format.fprintf fmt "Program has terminated.@."
  | Cont c ->
    let input = read_line () in
    c fmt input |> continue_until_end fmt

let () =
  let fmt = Format.std_formatter in
  main fmt |> continue_until_end fmt
