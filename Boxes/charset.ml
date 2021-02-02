
type pkmn_char = Unused | Unreadable of string
               | Unavailable of string | Available of string

(* This corresponds to the charset of English Emerald *)
let charset =
  [|
  (* 0x0. *)
  Available   "␣" ; Unavailable "À" ; Unavailable "Á" ; Unavailable "Â" ;
  Unavailable "Ç" ; Unavailable "È" ; Unavailable "É" ; Unavailable "Ê" ;
  Unavailable "Ë" ; Unavailable "Ì" ; Unused          ; Unavailable "Î" ;
  Unavailable "Ï" ; Unavailable "Ò" ; Unavailable "Ó" ; Unavailable "Ô" ;
  (* 0x1. *)
  Unavailable "Œ" ; Unavailable "Ù" ; Unavailable "Ú" ; Unavailable "Û" ;
  Unavailable "Ñ" ; Unavailable "ß" ; Unavailable "à" ; Unavailable "á" ;
  Unused          ; Unavailable "ç" ; Unavailable "è" ; Unavailable "é" ;
  Unavailable "ê" ; Unavailable "ë" ; Unavailable "ì" ; Unused          ;
  (* 0x2. *)
  Unavailable "î"  ; Unavailable "ï" ; Unavailable "ò" ; Unavailable "ó" ;
  Unavailable "ô"  ; Unavailable "œ" ; Unavailable "ù" ; Unavailable "ú" ;
  Unavailable "û"  ; Unavailable "ñ" ; Unavailable "º" ; Unavailable "ª" ;
  Unavailable "ᵉʳ" ; Unavailable "&" ; Unavailable "+" ; Unused          ;
  (* 0x3. *)
  Unused           ; Unused          ; Unused          ; Unused          ;
  Unavailable "Lv" ; Unavailable "=" ; Unavailable ";" ; Unused          ;
  Unused           ; Unused          ; Unused          ; Unused          ;
  Unused           ; Unused          ; Unused          ; Unused          ;
  (* 0x4. *)
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unused          ;
  (* 0x5. *)
  Unavailable "▯"   ; Unavailable "¿"    ; Unavailable "¡"  ; Unavailable "PK"   ;
  Unavailable "MN"   ; Unavailable "PO"   ; Unavailable "Ké" ; Unreadable "0x57"  ;
  Unreadable  "0x58" ; Unreadable "0x59"  ; Unavailable "Í"  ; Unavailable "%"    ;
  Unavailable "("    ; Unavailable ")"    ; Unused           ; Unused             ;
  (* 0x6. *)
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unavailable "â" ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unavailable "í" ;
  (* 0x7. *)
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unavailable "⬆" ; Unavailable "⬇" ; Unavailable "⬅" ;
  Unavailable "➡" ; Unavailable "*" ; Unavailable "*" ; Unavailable "*" ;
  (* 0x8. *)
  Unavailable "*" ; Unavailable "*" ; Unavailable "*" ; Unavailable "*" ;
  Unavailable "ᵉ" ; Unavailable "<" ; Unavailable ">" ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unused          ;
  (* 0x9. *)
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unused          ;
  Unused          ; Unused          ; Unused          ; Unused          ;
  (* 0xA. *)
  Unavailable "ʳᵉ" ; Available "0"   ; Available "1" ; Available "2"    ;
  Available "3"    ; Available "4"   ; Available "5" ; Available "6"    ;
  Available "7"    ; Available "8"   ; Available "9" ; Available "!"    ;
  Available "?"    ; Available "."   ; Available "–" ; Unavailable "・" ;
  (* 0xB. *)
  Available "…"    ; Available "“"   ; Available "”" ; Available "‘"     ;
  Available "’"    ; Available "♂"   ; Available "♀" ; Unavailable "Pk$" ;
  Available ","    ; Unavailable "×" ; Available "/" ; Available "A"     ;
  Available "B"    ; Available "C"   ; Available "D" ; Available "E"     ;
  (* 0xC. *)
  Available "F"    ; Available "G"   ; Available "H" ; Available "I"     ;
  Available "J"    ; Available "K"   ; Available "L" ; Available "M"     ;
  Available "N"    ; Available "O"   ; Available "P" ; Available "Q"     ;
  Available "R"    ; Available "S"   ; Available "T" ; Available "U"     ;
  (* 0xD. *)
  Available "V"    ; Available "W"   ; Available "X" ; Available "Y"     ;
  Available "Z"    ; Available "a"   ; Available "b" ; Available "c"     ;
  Available "d"    ; Available "e"   ; Available "f" ; Available "g"     ;
  Available "h"    ; Available "i"   ; Available "j" ; Available "k"     ;
  (* 0xE. *)
  Available "l"    ; Available "m"   ; Available "n" ; Available "o"     ;
  Available "p"    ; Available "q"   ; Available "r" ; Available "s"     ;
  Available "t"    ; Available "u"   ; Available "v" ; Available "w"     ;
  Available "x"    ; Available "y"   ; Available "z" ; Unavailable "▶"  ;
  (* 0xF. *)
  Unavailable ":"   ; Unavailable "Ä"   ; Unavailable "Ö"   ; Unavailable "Ü"   ;
  Unavailable "ä"   ; Unavailable "ö"   ; Unavailable "ü"   ; Unused            ;
  Unused            ; Unused            ; Unreadable "0xFA" ; Unreadable "0xFB" ;
  Unreadable "0xFC" ; Unreadable "0xFD" ; Unreadable "0xFE" ; Unreadable "0xFF" ;
  |]

let is_code_available code =
  match charset.(code) with
  | Available _ -> true
  | Unavailable _ | Unreadable _ | Unused -> false

let is_code_readable code =
  match charset.(code) with
  | Available _ | Unavailable _ -> true
  | Unreadable _ | Unused -> false

let is_code_used code =
  match charset.(code) with
  | Available _ | Unavailable _ | Unreadable _ -> true
  | Unused -> false

let invalid_str = "❌"

let char_at code =
  match charset.(code) with
  | Available str | Unavailable str | Unreadable str -> str
  | Unused -> invalid_str

let readable_char_at code =
  match charset.(code) with
  | Available str | Unavailable str -> str
  | Unreadable _ | Unused -> invalid_str

let writable_char_at code =
  match charset.(code) with
  | Available str -> str
  | Unavailable _ | Unreadable _ | Unused -> invalid_str
