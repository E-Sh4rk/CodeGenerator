
type t

val from_str : string -> t
val from_filename : string -> t

val main_file : t -> Parse.parsed_content
val get_file : string -> t -> Parse.parsed_content
