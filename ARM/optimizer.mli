
exception CannotOptimize

(* NOTE: The optimizer assumes that the Carry flag is unset.
   The generated commands will not set the Carry flag. *)

type optimization_setting =
  | NoOptimization
  | FixedLength of int
  | VariableLength

val fix_arm : (Arm.arm * optimization_setting) list -> Arm.arm list
val do_not_fix_arm : (Arm.arm * optimization_setting) list -> Arm.arm list

(* TODO: ADC (and SBC) must sometimes have the S flag... *)
