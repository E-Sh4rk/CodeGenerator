open Seed

let stationnary =
  [ "Deoxys, Mew, Rayquaza, Kyogre, Groudon, Regirock, Registeel, Regice, Lati@s, Lugia, Ho-Oh", 3, "" ;
    "Fossils", 3, "" ;
    "Sudowoodo", 6, "3 + 3 for the menu navigation involved for using the Wailmer Pail" ;
    "Beldum", 4, "" ;
    "Kecleon", 4, "" ;
    "Electrode (both Electrode are present)", 5, "" ;
    "Electrode (only one Electrode is present)", 4, "" ;
    "Hoenn starter", 7, "" ;
    "Johto starter", 8, "" ;
  ]

type result = 
  | Cont of (Format.formatter -> string -> result)
  | NoCont

let iter f n x =
  List.init n (fun _ -> 0) |> List.fold_left (fun acc _ -> f acc) x

let rec main fmt =
  Format.fprintf fmt "To determine your target seed, please use a recent version of PokeFinder (>= 4.1.2).@." ;
  Format.fprintf fmt "Please enter your target seed (use 0x prefix if hexadecimal): @?" ;
  Cont main_1

and main_1 fmt str =
  let seed = Utils.uint32_of_str str in
  Format.fprintf fmt "To determine the seed to use with the PRNG stall ACE code, please enter your method:@." ;
  Format.fprintf fmt "1. Method H1 (wild pokemon) using sweet scent.@." ;
  Format.fprintf fmt "2. Method H1 (wild pokemon) using a rod.@." ;
  Format.fprintf fmt "3. Method 1 (stationnary pokemon).@." ;
  Format.fprintf fmt "4. Other (show all the seeds in the vicinity).@." ;
  Format.fprintf fmt "5. Choose another target seed.@." ;
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
    Format.fprintf fmt "Please select the pokemon you want:@." ;
    stationnary |> List.iteri (fun i (name,_,_) ->
      Format.fprintf fmt "%i. %s@." (i+1) name
    ) ;
    Format.fprintf fmt "%i. Other@." ((List.length stationnary) + 1) ;
    Cont (main_5 seed)
  | "4" ->
    Format.fprintf fmt "Please enter the range (example: -25 5):@." ;
    Cont (main_vicinity seed)
  | _ -> NoCont

and main_vicinity seed fmt str =
  let (start, stop) = Scanf.sscanf str " %i %i" (fun i j -> (i, j)) in
  show_vicinity fmt true seed start stop ;
  NoCont

and main_3 seed fmt str =
  let delay =
    match str with
    | "1" -> 4
    | "2" -> 2
    | "3" -> 3
    | _ -> failwith "Unknown answer."
  in
  let seed' = iter prev_seed delay seed in
  Format.fprintf fmt "You should use the seed %#lx (%n cycle(s) before your target).@." seed' delay ;
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
    match best_seed_for_rod route119 feebas (prev_seed seed) rod with
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

and main_5 seed fmt str =
  begin match int_of_string str with
  | n ->
    if n < 1 || n > (List.length stationnary)
    then begin
      Format.fprintf fmt "For most legendaries, you should use the seed at cycle -3.@." ;
      show_vicinity fmt false seed (-10) 0
    end else begin
      let (_, offset, _) = List.nth stationnary (n-1) in
      let seed' = iter prev_seed offset seed in
      Format.fprintf fmt "You should use the seed %#lx (%n cycle(s) before your target).@." seed' offset ;
    end ;
    Format.fprintf fmt "You should start the battle as soon as you can" ;
    Format.fprintf fmt " (just after having executed the ACE and closed the menu).@." ;
  | exception (Failure _) -> failwith "Unknown answer."
  end ; NoCont
