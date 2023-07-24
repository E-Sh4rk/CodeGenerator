
val is_code_available : int -> bool
val is_code_readable : int -> bool
val is_code_used : int -> bool

val spacing_char : string
val invalid_char : string
val char_at : int -> string
val readable_char_at : int -> string
val writable_char_at : int -> string

val all_writable_chars : string list
val encode_writable_char : string -> int (* Can raise Not_found *)
