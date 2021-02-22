
exception InvalidSave

val box_names_section_id : int
val team_items_section_id : int

val extract_box_names_from_section : Bytes.t -> Bytes.t
val update_box_names : Bytes.t -> Bytes.t -> unit

val extract_team_from_section : Bytes.t -> Bytes.t list
val update_team : Bytes.t -> Bytes.t list -> unit

val write_section : out_channel -> int -> Bytes.t -> unit
val read_section : in_channel -> int -> int * Bytes.t
