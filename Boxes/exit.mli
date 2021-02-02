
type t

val load_from_dir : string -> t
val load_from_file : string -> t
val load_from_parsed_file : Parse.parsed_content -> t

exception NoExitCode

val get_preferred : t -> int -> int * ((int list) list)
