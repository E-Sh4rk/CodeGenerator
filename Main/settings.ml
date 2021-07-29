
type languages = ENG | FRA | ITA | SPA | GER | JAP
let lang = ref ENG

type tweaker_modes = Strict | Flexible
let tweaker_mode = ref Flexible
(* TODO: Take this setting into account:

  let strict =
    match !Settings.tweaker_mode with
    | Settings.Flexible -> false | Settings.Strict -> true
  in
  ...
  
  When strict, the S flag must be set to false except for the last command generated
  for which it should match the S flag of the inital command.
  Moreover, the ADD and SUB commands should be used instead of ADC and SBC.
*)

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
