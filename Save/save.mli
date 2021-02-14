
exception InvalidSave

val read_box_names : in_channel -> Bytes.t
val read_box_names_section : in_channel -> int * Bytes.t

val update_box_names : Bytes.t -> Bytes.t -> unit
val write_section : out_channel -> int -> Bytes.t -> unit
