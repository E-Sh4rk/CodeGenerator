
type pokemon = { pid:int32 ; otid:int32 }

val tid_from_vid_sid : int32 -> int32 -> int32
val ivea_offset : pokemon -> int
val decrypt_aligned_int32 : pokemon -> int32 -> int32
