
let () =
  Format.printf "Please enter PID: @?" ;
  let pid = read_line () |> Int32.of_string in
  Format.printf "Please enter original trainer visible TID: @?" ;
  let vid = read_line () |> Int32.of_string in
  Format.printf "Please enter original trainer secret TID: @?" ;
  let sid = read_line () |> Int32.of_string in
  let tid = Structure.tid_from_vid_sid vid sid in
  let pkmn = { Structure.pid=pid ; Structure.otid=tid } in
  let ivea_offset = Structure.ivea_offset pkmn in
  Format.printf "1st pokemon IVEA offset: %#x@." ivea_offset ;
  Format.printf "2nd pokemon IVEA offset: %#x@."
    (ivea_offset + Constants.party_pkmn_data_size) ;
  Format.printf "Party address: %#x@." Constants.party_offset_addr_eng