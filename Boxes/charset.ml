
type pkmn_char = Unused | Unreadable of string
               | Unavailable of string | Available of string

let charset_eng =
  [|
  (* 0x0. *)
  Available   "_" ; Unavailable "À" ; Unavailable "Á" ; Unavailable "Â" ;
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

let charset_ita = Array.copy charset_eng
let charset_spa = Array.copy charset_eng
let charset_ger =
  let cs = Array.copy charset_eng in
  cs.(0xB1) <- Available "„" ;
  cs.(0xB2) <- Available "“" ;
  for i=0xF1 to 0xF6 do
    cs.(i) <- match cs.(i) with
      | Unavailable c -> Available c
      | _ -> assert false
  done ;
  cs
let charset_fra =
  let cs = Array.copy charset_eng in
  cs.(0xB1) <- Available "«" ;
  cs.(0xB2) <- Available "»" ;
  cs

let charset_jap =
  [|
  (* 0x0. *)
  Available "_"  ; Available "あ" ; Available "い" ; Available "う" ;
  Available "え" ; Available "お" ; Available "か" ; Available "き" ;
  Available "く" ; Available "け" ; Available "こ" ; Available "さ" ;
  Available "し" ; Available "す" ; Available "せ" ; Available "そ" ;
  (* 0x1. *)
  Available "た" ; Available "ち" ; Available "つ" ; Available "て" ;
  Available "と" ; Available "な" ; Available "に" ; Available "ぬ" ;
  Available "ね" ; Available "の" ; Available "は" ; Available "ひ" ;
  Available "ふ" ; Available "へ" ; Available "ほ" ; Available "ま" ;
  (* 0x2. *)
  Available "み" ; Available "む" ; Available "め" ; Available "も" ;
  Available "や" ; Available "ゆ" ; Available "よ" ; Available "ら" ;
  Available "り" ; Available "る" ; Available "れ" ; Available "ろ" ;
  Available "わ" ; Available "を" ; Available "ん" ; Available "ぁ" ;
  (* 0x3. *)
  Available "ぃ" ; Available "ぅ" ; Available "ぇ" ; Available "ぉ" ;
  Available "ゃ" ; Available "ゅ" ; Available "ょ" ; Available "が" ;
  Available "ぎ" ; Available "ぐ" ; Available "げ" ; Available "ご" ;
  Available "ざ" ; Available "じ" ; Available "ず" ; Available "ぜ" ;
  (* 0x4. *)
  Available "ぞ" ; Available "だ" ; Available "ぢ" ; Available "づ" ;
  Available "で" ; Available "ど" ; Available "ば" ; Available "び" ;
  Available "ぶ" ; Available "べ" ; Available "ぼ" ; Available "ぱ" ;
  Available "ぴ" ; Available "ぷ" ; Available "ぺ" ; Available "ぽ" ;
  (* 0x5. *)
  Available "っ" ; Available "ア" ; Available "イ" ; Available "ウ" ;
  Available "エ" ; Available "オ" ; Available "カ" ; Available "キ" ;
  Available "ク" ; Available "ケ" ; Available "コ" ; Available "サ" ;
  Available "シ" ; Available "ス" ; Available "セ" ; Available "ソ" ;
  (* 0x6. *)
  Available "タ" ; Available "チ" ; Available "ツ" ; Available "テ" ;
  Available "ト" ; Available "ナ" ; Available "ニ" ; Available "ヌ" ;
  Available "ネ" ; Available "ノ" ; Available "ハ" ; Available "ヒ" ;
  Available "フ" ; Available "ヘ" ; Available "ホ" ; Available "マ" ;
  (* 0x7. *)
  Available "ミ" ; Available "ム" ; Available "メ" ; Available "モ" ;
  Available "ヤ" ; Available "ユ" ; Available "ヨ" ; Available "ラ" ;
  Available "リ" ; Available "ル" ; Available "レ" ; Available "ロ" ;
  Available "ワ" ; Available "ヲ" ; Available "ン" ; Available "ァ" ;
  (* 0x8. *)
  Available "ィ" ; Available "ゥ" ; Available "ェ" ; Available "ォ" ;
  Available "ャ" ; Available "ュ" ; Available "ョ" ; Available "ガ" ;
  Available "ギ" ; Available "グ" ; Available "ゲ" ; Available "ゴ" ;
  Available "ザ" ; Available "ジ" ; Available "ズ" ; Available "ゼ" ;
  (* 0x9. *)
  Available "ゾ" ; Available "ダ" ; Available "ヂ" ; Available "ヅ" ;
  Available "デ" ; Available "ド" ; Available "バ" ; Available "ビ" ;
  Available "ブ" ; Available "ベ" ; Available "ボ" ; Available "パ" ;
  Available "ピ" ; Available "プ" ; Available "ペ" ; Available "ポ" ;
  (* 0xA. *)
  Available "ッ"   ; Available "0"   ; Available "1" ; Available "2"    ;
  Available "3"    ; Available "4"   ; Available "5" ; Available "6"    ;
  Available "7"    ; Available "8"   ; Available "9" ; Available "！"   ;
  Available "？"   ; Available "。"  ; Available "ー"; Available "・"    ;
  (* 0xB. *)
  Available "‥"    ; Available "『"  ; Available "』"; Available "「"    ;
  Available "」"   ; Available "♂"   ; Available "♀" ; Unavailable "円"  ;
  Unavailable "."  ; Unavailable "×" ; Available "/" ; Available "A"     ;
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

let charset () =
  match !Settings.lang with
  | ENG -> charset_eng
  | FRA -> charset_fra
  | ITA -> charset_ita
  | SPA -> charset_spa
  | GER -> charset_ger
  | JAP -> charset_jap

let is_code_available code =
  match (charset ()).(code) with
  | Available _ -> true
  | Unavailable _ | Unreadable _ | Unused -> false

let is_code_readable code =
  match (charset ()).(code) with
  | Available _ | Unavailable _ -> true
  | Unreadable _ | Unused -> false

let is_code_used code =
  match (charset ()).(code) with
  | Available _ | Unavailable _ | Unreadable _ -> true
  | Unused -> false

let spacing_char = "_"
let invalid_char = "✖"

let char_at code =
  match (charset ()).(code) with
  | Available str | Unavailable str | Unreadable str -> str
  | Unused -> invalid_char

let readable_char_at code =
  match (charset ()).(code) with
  | Available str | Unavailable str -> str
  | Unreadable _ | Unused -> invalid_char

let writable_char_at code =
  match (charset ()).(code) with
  | Available str -> str
  | Unavailable _ | Unreadable _ | Unused -> invalid_char

let all_writable_chars =
  let rec aux acc i =
    if i < 0 then acc
    else
      match (charset ()).(i) with
      | Available str -> aux (str::acc) (i-1)
      | Unavailable _ | Unreadable _ | Unused -> aux acc (i-1)
  in
  aux [] 0xFF

let encode_writable_char str =
  let rec aux i =
    if i < 0 then raise Not_found
    else
      match (charset ()).(i) with
      | Available str' when String.equal str str' -> i
      | Available _ | Unavailable _ | Unreadable _ | Unused -> aux (i-1)
  in
  aux 0xFF
