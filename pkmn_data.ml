
let () =
  Format.printf "Please enter low-PID: @?" ;
  let lpid = read_line () |> Int32.of_string in
  Format.printf "Please enter high-PID: @?" ;
  let hpid = read_line () |> Int32.of_string in
  let pid = Structure.int32_from_low_high lpid hpid in

  Format.printf "Please enter original trainer visible TID: @?" ;
  let vid = read_line () |> Int32.of_string in
  Format.printf "Please enter original trainer secret TID: @?" ;
  let sid = read_line () |> Int32.of_string in
  let tid = Structure.tid_from_vid_sid vid sid in

  let pkmn = { Structure.pid=pid ; Structure.otid=tid } in
  let misc_pos = Structure.substructure_position pkmn 'M' in
  let ivea_offset = Structure.ivea_offset pkmn in
  Format.printf "Misc substructure position: %d@." (misc_pos+1) ;
  Format.printf "IVEA offset: %#x@." ivea_offset ;

  Format.printf "Please enter low-IVEA: @?" ;
  let livea = read_line () |> Int32.of_string in
  Format.printf "Please enter high-IVEA: @?" ;
  let hivea = read_line () |> Int32.of_string in
  let ivea = Structure.int32_from_low_high livea hivea in
  let uivea = Structure.decrypt_aligned_int32 pkmn ivea in
  Format.printf "Unencrypted IVEA data: %#lx@." uivea ;
  let (hp, atk, def, speed, sp_atk, sp_def, data) = Structure.ivea_data_to_ivs uivea in
  Format.printf "Pokemon IVs (hp atk def speed sp_atk sp_def): %ld %ld %ld %ld %ld %ld@."
    hp atk def speed sp_atk sp_def ;

  Format.printf "Maxing IVs...@." ;
  let iv = Int32.of_int 31 in
  let uivea' = Structure.ivs_to_ivea_data (iv, iv, iv, iv, iv, iv, data) in
  Format.printf "New unencrypted IVEA data: %#lx@." uivea' ;
  let ivea = Structure.encrypt_aligned_int32 pkmn uivea' in
  let (livea, hivea) = Structure.int32_to_low_high ivea in
  Format.printf "New low-IVEA: %#lx@." livea ;
  Format.printf "New high-IVEA: %#lx@." hivea ;
  let checksum_diff = Structure.checksum_diff_for_aligned_int32 uivea uivea' in
  Format.printf "Checksum diff: %#lx@." checksum_diff
