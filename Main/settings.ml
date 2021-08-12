
type languages = ENG | FRA | ITA | SPA | GER | JAP
let lang = ref ENG

type tweaker_modes = Strict | Flexible
let tweaker_mode = ref Flexible

let configure language =
  tweaker_mode := Flexible ;
  match language with
  | "eng" -> lang := ENG
  | "spa" -> lang := SPA
  | "fra" -> lang := FRA
  | "ita" -> lang := ITA
  | "ger" -> lang := GER
  | "jap" -> lang := JAP ; tweaker_mode := Strict
  | _ -> lang := ENG
