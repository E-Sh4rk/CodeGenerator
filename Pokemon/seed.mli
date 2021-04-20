
val prev_seed : int32 -> int32
val next_seed : int32 -> int32
val cycle_to : int32 -> int32
val seed_at : int32 -> int32

val old_rod : int
val good_rod : int
val super_rod : int
val best_seed_for_rod : bool -> bool -> int32 -> int -> (bool*int*int32) option