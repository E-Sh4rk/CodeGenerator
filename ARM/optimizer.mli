
exception CannotOptimize

val fix_arm : (Arm.arm * bool) list -> Arm.arm list
val do_not_fix_arm : (Arm.arm * bool) list -> Arm.arm list
