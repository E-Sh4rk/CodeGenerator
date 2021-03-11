
exception CannotOptimize

(* NOTE: The optimizer assumes that the Carry flag is unset.
   The generated commands will not set the Carry flag. *)

type tweaking_settings =
  | NoTweaking
  | TweakFixedLength of int
  | TweakMinLength

val synthesis_test : int -> int32 -> (int32 list * bool) option

val tweak_arm : (Arm.arm * tweaking_settings) list -> Arm.arm list
val do_not_tweak_arm : (Arm.arm * tweaking_settings) list -> Arm.arm list
