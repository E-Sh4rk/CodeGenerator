
type result = 
  | Cont of (Format.formatter -> string -> result)
  | NoCont

let rec main fmt =
  Format.fprintf fmt "Please enter low-PID: @?" ;
  Cont main_1

and main_1 fmt str =
  let lpid = Scanf.sscanf str " %li" (fun i -> i) in
  Format.fprintf fmt "Please enter high-PID: @?" ;
  Cont (main_2 lpid)

and main_2 lpid fmt str =
  let hpid = Scanf.sscanf str " %li" (fun i -> i) in
  let pid = Structure.int32_from_low_high lpid hpid in

  Format.fprintf fmt "Please enter original trainer visible TID: @?" ;
  Cont (main_3 pid)

and main_3 pid fmt str =
  let vid = Scanf.sscanf str " %li" (fun i -> i) in
  Format.fprintf fmt "Please enter original trainer secret TID: @?" ;
  Cont (main_4 pid vid)

and main_4 pid vid fmt str =
  let sid = Scanf.sscanf str " %li" (fun i -> i) in
  let tid = Structure.int32_from_low_high vid sid in

  let pkmn = { Structure.pid=pid ; Structure.otid=tid } in
  let misc_pos = Structure.substructure_position pkmn 'M' in
  let ivea_offset = Structure.ivea_offset pkmn in
  Format.fprintf fmt "Misc substructure position: %d@." (misc_pos+1) ;
  Format.fprintf fmt "IVEA offset: %#x@." ivea_offset ;

  Format.fprintf fmt "Please enter low-IVEA: @?" ;
  Cont (main_5 pkmn)

and main_5 pkmn fmt str =
  let livea = Scanf.sscanf str " %li" (fun i -> i) in
  Format.fprintf fmt "Please enter high-IVEA: @?" ;
  Cont (main_6 pkmn livea)

and main_6 pkmn livea fmt str =
  let hivea = Scanf.sscanf str " %li" (fun i -> i) in
  let ivea = Structure.int32_from_low_high livea hivea in
  let uivea = Structure.decrypt_aligned_int32 pkmn ivea in
  Format.fprintf fmt "Unencrypted IVEA data: %#lx@." uivea ;
  let (hp, atk, def, speed, sp_atk, sp_def, data) = Structure.ivea_data_to_ivs uivea in
  Format.fprintf fmt "Pokemon IVs (hp atk def speed sp_atk sp_def): %ld %ld %ld %ld %ld %ld@."
    hp atk def speed sp_atk sp_def ;

  Format.fprintf fmt "Maxing IVs...@." ;
  let iv = Int32.of_int 31 in
  let uivea' = Structure.ivs_to_ivea_data (iv, iv, iv, iv, iv, iv, data) in
  Format.fprintf fmt "New unencrypted IVEA data: %#lx@." uivea' ;
  let ivea = Structure.encrypt_aligned_int32 pkmn uivea' in
  let (livea, hivea) = Structure.int32_to_low_high ivea in
  Format.fprintf fmt "New low-IVEA: %#lx@." livea ;
  Format.fprintf fmt "New high-IVEA: %#lx@." hivea ;
  let checksum_diff = Structure.checksum_diff_for_aligned_int32 uivea uivea' in
  Format.fprintf fmt "Checksum diff: %#lx@." checksum_diff ;
  NoCont
