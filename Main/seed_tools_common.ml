open Seed

type result = 
  | Cont of (Format.formatter -> string -> result)
  | NoCont

let rec main fmt =
  Format.fprintf fmt "Please enter your seed (use 0x prefix if hexadecimal): @?" ;
  Cont main_1

and main_1 fmt str =
  let seed = Utils.uint32_of_str str in
  Format.fprintf fmt "Do you want to obtain this seed using the freeze PRNG ACE code ?@." ;
  Format.fprintf fmt "1. Yes, for method H1 (wild pokemon) using sweet scent.@." ;
  Format.fprintf fmt "2. Yes, for method H1 (wild pokemon) using a rod.@." ;
  Format.fprintf fmt "3. Yes, for method 1 (stationnary pokemon).@." ;
  Format.fprintf fmt "4. No, but please show me the seeds in the vicinity.@." ;
  Format.fprintf fmt "5. No (quit).@." ;
  Cont (main_2 seed)

and show_vicinity fmt print_cycle seed start stop =
  let cycle = cycle_to seed in
  if print_cycle then Format.fprintf fmt "Cycle from seed 0: %lu@." cycle ;
  let start_seed = ref (seed_at (Int32.add cycle (Int32.of_int start))) in
  for i = start to stop do
    Format.fprintf fmt "%i: %lu (%#lx)@." i (!start_seed) (!start_seed) ;
    start_seed := next_seed (!start_seed)
  done

and main_2 seed fmt str =
  match str with
  | "1" ->
    Format.fprintf fmt "Please select your configuration:@." ;
    Format.fprintf fmt "1. I will be in the Safari Zone.@." ;
    Format.fprintf fmt "2. I will use a custom mass outbreak.@." ;
    Format.fprintf fmt "3. None of the above.@." ;
    Cont (main_3 seed)
  | "2" ->
    Format.fprintf fmt "Please select your configuration:@." ;
    Format.fprintf fmt "1. I will not be fishing on route 119.@." ;
    Format.fprintf fmt "2. I will be fishing on route 119, but not for a feebas.@." ;
    Format.fprintf fmt "3. I will be fishing for feebas on a feebas tile.@." ;
    Cont (main_4 seed)
  | "3" ->
    Format.fprintf fmt "For most legendaries, you should use the seed at cycle -3.@." ;
    Format.fprintf fmt "Please refer to the instructions on the freeze PRNG ACE code for other stationnary pokemons (pokeballs, Sudowoodo, starters...).@." ;
    Format.fprintf fmt "You should start the battle as soon as you can" ;
    Format.fprintf fmt " (just after having executed the ACE and closed the menu).@." ;
    show_vicinity fmt false seed (-10) 0 ;
    NoCont
  | "4" ->
    Format.fprintf fmt "Please enter the range (example: -25 5):@." ;
    Cont (main_vicinity seed)
  | _ -> NoCont

and main_vicinity seed fmt str =
  let (start, stop) = Scanf.sscanf str " %i %i" (fun i j -> (i, j)) in
  show_vicinity fmt true seed start stop ;
  NoCont

and main_3 seed fmt str =
  let (delay, seed') =
    match str with
    | "1" -> (3, prev_seed (prev_seed (prev_seed seed)))
    | "2" -> (1, prev_seed seed)
    | "3" -> (2, prev_seed (prev_seed seed))
    | _ -> failwith "Unknown answer."
  in
  Format.fprintf fmt "You should use the seed %#lx (%n cycle(s) before your target).@."
    seed' delay ;
  Format.fprintf fmt "You should use sweet scent directly after triggering the ACE," ;
  Format.fprintf fmt " without closing the pokemon menu.@." ;
  NoCont

and main_4 seed fmt str =
  let (route119, feebas) =
    match str with
    | "1" -> (false, false)
    | "2" -> (true, false)
    | "3" -> (true, true)
    | _ -> failwith "Unknown answer."
  in
  for rod = old_rod to super_rod do
    let rodname =
      match rod with
      | r when r=old_rod -> "Old rod"
      | r when r=good_rod -> "Good rod"
      | r when r=super_rod -> "Super rod"
      | _ -> assert false
    in
    Format.fprintf fmt "%s:@." rodname ;
    match best_seed_for_rod route119 feebas seed rod with
    | None -> Format.fprintf fmt "\tNo matches found@."
    | Some (false, adv, seed) ->
      Format.fprintf fmt "\tWith lead: No lead necessary@." ;
      Format.fprintf fmt "\tUse seed %#lx to generate target on advancement %i@." seed adv
    | Some (true, adv, seed) ->
      Format.fprintf fmt "\tWith lead: Must have Suction Cup or Sticky Hold lead@." ;
      Format.fprintf fmt "\tUse seed %#lx to generate target on advancement %i@." seed adv
  done ;
  Format.fprintf fmt "You should use the rod directly after triggering the ACE" ;
  Format.fprintf fmt " (leave the pokemon menu and enter the bag).@." ;
  Format.fprintf fmt "Thanks to Shao for this script.@." ;
  NoCont