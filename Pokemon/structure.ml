open Int32

type pokemon = { pid:int32 ; otid:int32 }

let tid_from_vid_sid v s =
  logor v (shift_left s 16)

let decryption_key { pid ; otid ; _ } = logxor pid otid

let substructures_order = [|
  "GAEM" ; "GAME" ; "GEAM" ; "GEMA" ; "GMAE" ; "GMEA" ;
  "AGEM" ; "AGME" ; "AEGM" ; "AEMG" ; "AMGE" ; "AMEG" ;
  "EGAM" ; "EGMA" ; "EAGM" ; "EAMG" ; "EMGA" ; "EMAG" ;
  "MGAE" ; "MGEA" ; "MAGE" ; "MAEG" ; "MEGA" ; "MEAG" ;
|]

let substructure_position { pid ; _ } ss =
  let i = unsigned_rem pid (of_int 24) |> Name.int32_to_int in
  let order = substructures_order.(i) in
  let rec aux i =
    if order.[i] = ss then i else aux (i+1)
  in
  aux 0

let data_offset = 32
let substructure_size = 12

let substructure_offset pkmn ss =
  let p = substructure_position pkmn ss in
  data_offset + substructure_size*p

let ivea_offset pkmn =
  (substructure_offset pkmn 'M') + 4

let decrypt_aligned_int32 pkmn i =
  let key = decryption_key pkmn in
  logxor i key
