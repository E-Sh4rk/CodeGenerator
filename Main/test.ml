
(* 0x20244ecl *)

let () =
  Format.printf "Starting...@." ;
  match Optimizer.synthesis_test 5 0x20244edl with
  | None -> Format.printf "Fail.@."
  | Some (lst, _) -> Format.printf "Success: %i@." (List.length lst)
