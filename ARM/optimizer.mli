
exception CannotOptimize

(* NOTE: The optimizer assumes that the Carry flag is unset.
   The generated commands will not set the Carry flag. *)

val fix_arm : (Arm.arm * bool) list -> Arm.arm list
val do_not_fix_arm : (Arm.arm * bool) list -> Arm.arm list
