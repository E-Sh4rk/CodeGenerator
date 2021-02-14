
exception InvalidSave

let game_save_A = 0x0000
let game_save_B = 0xE000

let section_size = 0x1000
let nb_sections = 14
let section_id_offset = 0xFF4
let checksum_offset = 0xFF6
let save_index_offset = 0xFFC

let pc_buffers_data_size = 3968
let box_names_section_id = 13 (* PC buffer I *)
let box_names_section_data_length = 2000
let box_names_offset = 0x8344 - 8*pc_buffers_data_size
let box_names_length = 126

let addr_of_section inc id =
  let rbuf = Bytes.create section_size in
  let rec aux base i =
    if i >= nb_sections
    then raise InvalidSave
    else
      let addr = base + i*section_size in
      seek_in inc addr ; really_input inc rbuf 0 section_size ;
      let id' = Bytes.get_uint16_le rbuf section_id_offset in
      let index = Bytes.get_int32_le rbuf save_index_offset in
      if id' = id
      then (addr, index)
      else aux base (i+1)
  in
  let (addra, indexa) = aux game_save_A 0 in
  let (addrb, indexb) = aux game_save_B 0 in
  if Int32.unsigned_compare indexa indexb < 0
  then addrb else addra

let read_box_names inc =
  let addr = addr_of_section inc box_names_section_id in
  let addr = addr + box_names_offset in
  let res = Bytes.create box_names_length in
  seek_in inc addr ; really_input inc res 0 box_names_length ;
  res

let read_box_names_section inc =
  let addr = addr_of_section inc box_names_section_id in
  let res = Bytes.create section_size in
  seek_in inc addr ; really_input inc res 0 section_size ;
  (addr, res)

let mask16 = Int32.of_int 0xFFFF

let compute_checksum buf start len =
  let rec aux acc i =
    if i >= start+len
    then acc
    else
      let nb = Bytes.get_int32_le buf i in
      aux (Int32.add acc nb) (i+4)
  in
  let sum = aux Int32.zero start in
  let high = Int32.shift_right_logical sum 16 in
  let low = Int32.logand sum mask16 in
  let res = Int32.add high low in
  Int32.logand res mask16 |> Name.int32_to_int

let update_box_names buf box_names =
  let len = Bytes.length box_names in
  Bytes.blit box_names 0 buf box_names_offset len ;
  let checksum = compute_checksum buf 0 box_names_section_data_length in
  Bytes.set_uint16_le buf checksum_offset checksum

let write_section oc addr buf =
  seek_out oc addr ; output_bytes oc buf ; flush oc
