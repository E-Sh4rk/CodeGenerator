
val default_fillers : (int list) array

val fit_codes_into_boxes :
  ?fillers:((int list) array) ->
  ?start:int ->
  ?exit:(Exit.t option) ->
  (int list) list ->
  (int list) list

val pp_boxes_names : Format.formatter -> (int list) list -> unit
val pp_box_raw : Format.formatter -> int list -> unit

val nb_boxes : int
