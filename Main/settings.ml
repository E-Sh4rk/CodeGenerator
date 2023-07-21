
type languages = ENG | FRA | ITA | SPA | GER | JAP | ABC
let lang = ref ENG

type tweaker_modes = Strict | Flexible
let tweaker_mode = ref Flexible

type games = Emerald | FireRed | LeafGreen | Ruby | Sapphire
let game = ref Emerald

let hex_box_mode = ref false

let configure language game_version =
  tweaker_mode := Flexible ;
  hex_box_mode := false ;
  begin match String.sub language 0 3 with
  | "eng" -> lang := ENG
  | "spa" -> lang := SPA
  | "fra" -> lang := FRA
  | "ita" -> lang := ITA
  | "ger" -> lang := GER
  | "jap" -> lang := JAP ; tweaker_mode := Strict
  | "abc" -> lang := ABC ; tweaker_mode := Strict ; hex_box_mode := true
  | _ | exception Invalid_argument _ -> lang := ENG
  end ; match String.sub game_version 0 1 with
  | "e" -> game := Emerald
  | "f" -> game := FireRed
  | "l" -> game := LeafGreen
  | "r" -> game := Ruby
  | "s" -> game := Sapphire
  | _ | exception Invalid_argument _ -> game := Emerald
