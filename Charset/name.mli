
val eof : int
val space : int

val codes_for_command : int32 -> int list (* Bytes are reversed *)
val command_for_codes : int list -> int32 (* Bytes are reversed *)
val chars_for_command : int32 -> string list
val codes_to_chars : int list -> string list
val pp_chars : Format.formatter -> string list -> unit
val pp_chars_raw : Format.formatter -> string list -> unit

val is_code_writable : int list -> bool
val preferred_code : (int list) list -> int list

val is_full_of_spaces : int list -> bool