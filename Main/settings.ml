
type languages = ENG | FRA | ITA | SPA | GER | JAP | ABC
let lang = ref ENG

type tweaker_modes = Strict | Flexible
let tweaker_mode = ref Flexible

let hex_box_mode = ref false

let configure language =
  tweaker_mode := Flexible ;
  hex_box_mode := false ;
  match String.sub language 0 3 with
  | "eng" -> lang := ENG
  | "spa" -> lang := SPA
  | "fra" -> lang := FRA
  | "ita" -> lang := ITA
  | "ger" -> lang := GER
  | "jap" -> lang := JAP ; tweaker_mode := Strict
  | "abc" -> lang := ABC ; tweaker_mode := Strict ; hex_box_mode := true
  | _ -> lang := ENG
