open Int32

type pokemon = { pid:int32 ; otid:int32 }

let int32_from_low_high l h =
  logor l (shift_left h 16)

let mask16 = 0xFFFF |> of_int
let int32_to_low_high i =
  let l = logand mask16 i in
  let h = shift_right_logical i 16 in
  (l, h)

let data_offset = 32
let substructure_size = 12
let data_size = substructure_size * 4
let pid_offset = 0
let otid_offset = 4
let checksum_offset = 28

let decryption_key { pid ; otid ; _ } = logxor pid otid

let substructures_order = [|
  "GAEM" ; "GAME" ; "GEAM" ; "GEMA" ; "GMAE" ; "GMEA" ;
  "AGEM" ; "AGME" ; "AEGM" ; "AEMG" ; "AMGE" ; "AMEG" ;
  "EGAM" ; "EGMA" ; "EAGM" ; "EAMG" ; "EMGA" ; "EMAG" ;
  "MGAE" ; "MGEA" ; "MAGE" ; "MAEG" ; "MEGA" ; "MEAG" ;
|]

let pkmn_from_bytes buf =
  let pid = Bytes.get_int32_le buf pid_offset in
  let otid = Bytes.get_int32_le buf otid_offset in
  { pid ; otid }

let substructure_position { pid ; _ } ss =
  let i = unsigned_rem pid (of_int 24) |> Utils.uint32_to_int in
  let order = substructures_order.(i) in
  let rec aux i =
    if order.[i] = ss then i else aux (i+1)
  in
  aux 0

let substructure_offset pkmn ss =
  let p = substructure_position pkmn ss in
  data_offset + substructure_size*p

(* Local operations *)

let decrypt_aligned_int32 pkmn i =
  let key = decryption_key pkmn in
  logxor i key

let encrypt_aligned_int32 = decrypt_aligned_int32

let checksum_diff_for_aligned_int32 o n =
  let (lo, ho) = int32_to_low_high o in
  let (ln, hn) = int32_to_low_high n in
  let diff = add (sub ln lo) (sub hn ho) in
  logand diff mask16

(* Global operations *)

let enc_dec pkmn buf start len =
  let rec enc_dec_from offset = 
    if offset >= start + len then ()
    else (
      let i = Bytes.get_int32_le buf offset in
      let i = encrypt_aligned_int32 pkmn i in
      Bytes.set_int32_le buf offset i ;
      enc_dec_from (offset + 4)
    )
  in
  enc_dec_from start

let extract_data buf =
  let pkmn = pkmn_from_bytes buf in
  let res = Bytes.sub buf data_offset data_size in
  enc_dec pkmn res 0 data_size ;
  res

let compute_checksum buf start len =
  let rec aux acc i =
    if i >= start+len
    then acc
    else
      let nb = Bytes.get_uint16_le buf i |> Int32.of_int in
      aux (Int32.add acc nb) (i+2)
  in
  let sum = aux Int32.zero start in
  let low = Int32.logand sum mask16 in
  low |> Utils.uint32_to_int

let update_with_data buf data =
  let pkmn = pkmn_from_bytes buf in
  let checksum = compute_checksum data 0 data_size in
  Bytes.blit data 0 buf data_offset data_size ;
  enc_dec pkmn buf data_offset data_size ;
  Bytes.set_uint16_le buf checksum_offset checksum

(* Species *)

let species_offset pkmn =
  (substructure_offset pkmn 'G')

let species_offset_relative_to_data pkmn =
  (substructure_offset pkmn 'G') - data_offset

(* IVs *)

let ivea_offset pkmn =
  (substructure_offset pkmn 'M') + 4

let mask5 = 0b11111 |> of_int

let ivea_data_to_ivs data =
  let hp = logand mask5 data in
  let data = shift_right_logical data 5 in
  let atk = logand mask5 data in
  let data = shift_right_logical data 5 in
  let def = logand mask5 data in
  let data = shift_right_logical data 5 in
  let speed = logand mask5 data in
  let data = shift_right_logical data 5 in
  let sp_atk = logand mask5 data in
  let data = shift_right_logical data 5 in
  let sp_def = logand mask5 data in
  let data = shift_right_logical data 5 in
  (hp,atk,def,speed,sp_atk,sp_def,data)

let ivs_to_ivea_data (hp,atk,def,speed,sp_atk,sp_def,data) =
  let data = shift_left data 5 in
  let data = logor data sp_def in
  let data = shift_left data 5 in
  let data = logor data sp_atk in
  let data = shift_left data 5 in
  let data = logor data speed in
  let data = shift_left data 5 in
  let data = logor data def in
  let data = shift_left data 5 in
  let data = logor data atk in
  let data = shift_left data 5 in
  let data = logor data hp in
  data
