
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

let team_items_section_id = 1
let team_items_section_data_length = 3968
let team_size_offset = 0x234
let team_data_offset = 0x238
let pkmn_data_size = 100

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

let read_section inc section_id =
  let addr = addr_of_section inc section_id in
  let res = Bytes.create section_size in
  seek_in inc addr ; really_input inc res 0 section_size ;
  (addr, res)

let write_section oc addr buf =
  seek_out oc addr ; output_bytes oc buf ; flush oc

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
  Int32.logand res mask16 |> Utils.uint32_to_int

(* ----- BOX NAMES ----- *)

let extract_box_names_from_section buf =
  Bytes.sub buf box_names_offset box_names_length

let update_box_names buf box_names =
  let len = Bytes.length box_names in
  Bytes.blit box_names 0 buf box_names_offset len ;
  let checksum = compute_checksum buf 0 box_names_section_data_length in
  Bytes.set_uint16_le buf checksum_offset checksum

(* ----- TEAM ----- *)

let empty_pkmn () =
  Bytes.create pkmn_data_size

let extract_team_from_section buf =
  (*let nb = Bytes.get_int32_le buf team_size_offset |> Utils.uint32_to_int in
  let nb = if nb > 6 then 6 else nb in*)
  let nb = 6 in
  let rec extract_pkmns acc i =
    if i < 0 then acc
    else (
      let addr = team_data_offset + pkmn_data_size*i in
      let pkmn = Bytes.sub buf addr pkmn_data_size in
      extract_pkmns (pkmn::acc) (i-1)
    )
  in
  extract_pkmns [] (nb-1)

let update_team buf pkmns =
  let len = List.length pkmns |> Int32.of_int in
  Bytes.set_int32_le buf team_size_offset len ;
  let update_pkmn i pkmn =
    let addr = team_data_offset + pkmn_data_size*i in
    Bytes.blit pkmn 0 buf addr pkmn_data_size
  in
  List.iteri update_pkmn pkmns ;
  let checksum = compute_checksum buf 0 team_items_section_data_length in
  Bytes.set_uint16_le buf checksum_offset checksum
