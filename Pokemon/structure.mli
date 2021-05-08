
type pokemon = { pid:int32 ; otid:int32 }

val int32_from_low_high : int32 -> int32 -> int32
val int32_to_low_high : int32 -> int32 * int32

val pkmn_from_bytes : Bytes.t -> pokemon

val substructure_position : pokemon -> char -> int
val substructure_offset : pokemon -> char -> int

(* Global operations *)

val extract_data : Bytes.t -> Bytes.t
val update_with_data : Bytes.t -> Bytes.t -> unit

(* Local operations *)

val decrypt_aligned_int32 : pokemon -> int32 -> int32
val encrypt_aligned_int32 : pokemon -> int32 -> int32

val checksum_diff_for_aligned_int32 : int32 -> int32 -> int32

(* Species *)

val species_offset : pokemon -> int
val species_offset_relative_to_data : pokemon -> int

(* IVs *)

val ivea_offset : pokemon -> int

val ivea_data_to_ivs : int32 ->
  int32 * int32 * int32 * int32 * int32 * int32 * int32

val ivs_to_ivea_data :
  int32 * int32 * int32 * int32 * int32 * int32 * int32
  -> int32
