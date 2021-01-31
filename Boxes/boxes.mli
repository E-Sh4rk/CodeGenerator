
val fit_codes_into_boxes : ?exit:(Exit.t option) -> (int list) list -> (int list) list
val pp_boxes_names : Format.formatter -> (int list) list -> unit
val nop_code : int list

val nb_boxes : int
