
exception Unwritable

val codes_for_command : int32 -> int list (* Bytes are reversed *)
val chars_for_command : int32 -> string list
val codes_to_chars : int list -> string list
val pp_chars : Format.formatter -> string list -> unit

val is_code_writable : int list -> bool
val first_writable_code : (int list) list -> int list
