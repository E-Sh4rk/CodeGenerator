
type pokemon = { pid:int32 ; otid:int32 }

val int32_from_low_high : int32 -> int32 -> int32
val int32_to_low_high : int32 -> int32 * int32
val tid_from_vid_sid : int32 -> int32 -> int32

val substructure_position : pokemon -> char -> int
val ivea_offset : pokemon -> int
val decrypt_aligned_int32 : pokemon -> int32 -> int32
val encrypt_aligned_int32 : pokemon -> int32 -> int32

val ivea_data_to_ivs : int32 ->
  int32 * int32 * int32 * int32 * int32 * int32 * int32

val ivs_to_ivea_data :
  int32 * int32 * int32 * int32 * int32 * int32 * int32
  -> int32

val checksum_diff_for_aligned_int32 : int32 -> int32 -> int32
