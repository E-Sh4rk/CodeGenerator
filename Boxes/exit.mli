
type t

val load_from_dir : Preprocess.env -> string -> t
val load_from_file :
  Format.formatter -> Preprocess.env -> string -> Preprocess.headers * t
val load_from_parsed_file :
  Format.formatter -> Preprocess.env -> Parse.parsed_content -> t

exception NoExitCode

val get_preferred : t -> int -> int * ((int list) list)
