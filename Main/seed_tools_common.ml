open Seed

type result = 
  | Cont of (Format.formatter -> string -> result)
  | NoCont

let rec main fmt =
  Format.fprintf fmt "Please enter your seed (use 0x prefix if hexadecimal): @?" ;
  Cont main_1

and main_1 fmt str =
  let seed = Utils.uint32_of_str str in
  let cycle = cycle_to seed in
  Format.fprintf fmt "Cycle from seed 0: %lu@." cycle ;
  let start_seed = ref (seed_at (Int32.sub cycle 25l)) in
  for i = -25 to 10 do
    Format.fprintf fmt "%i: %lu (%#lx)@." i (!start_seed) (!start_seed) ;
    start_seed := next_seed (!start_seed)
  done ;
  NoCont
