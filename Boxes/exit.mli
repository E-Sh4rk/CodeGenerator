
type t

val load_from_dir : string -> t

exception NoExitCode

val get_preferred : t -> int -> int * ((int list) list)
