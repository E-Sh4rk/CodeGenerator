
exception BoxFittingError of string

type fillers =
  { nop_code:int list ; nop_code_alt:int list; fillers:int list array }
val default_fillers : unit -> fillers

val fit_codes_into_boxes :
  ?fill_last:bool ->
  ?fillers:fillers ->
  ?start:int ->
  ?exit:(Exit.t option) ->
  (int list) list ->
  (int list) list

val fit_codes_into_hex_boxes :
  ?exit:(Exit.t option) ->
  int list list ->
  int list list

val split_raw_into_boxes : ?fill_last:bool -> int list -> int list list

val pp_boxes_names : Format.formatter -> (int list) list -> unit
val pp_box_raw : Format.formatter -> int list -> unit

val nb_boxes : int
val name_size : int
