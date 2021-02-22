
let () =
  let filenames = IO_utils.enumerate_files (Sys.getcwd ()) ".sav" in
  filenames |> List.iteri (fun i str -> Format.printf "%i. %s@." i str) ;
  Format.printf "Your choice: @?" ;
  let filename = List.nth filenames (read_int ()) in
  let inc = open_in_gen [Open_rdonly;Open_binary] 0 filename in
  let current = Save.read_box_names inc in
  let len = Bytes.length current in
  let (addr, section) = Save.read_box_names_section inc in
  close_in inc ;
  Format.printf "Current data (in hexadecimal):@." ;
  current |> Bytes.iter (fun c -> Format.printf "%02X " (Char.code c)) ;
  Format.printf "@.Please enter new data (in hexadecimal):@." ;
  let line = read_line () in
  let inc_data = Scanf.Scanning.from_string line in
  let rec aux i =
    try
      let h = Scanf.bscanf inc_data " %X" (fun i -> i) in
      if i >= len then Format.printf "Warning: Data has been truncated@."
      else (
        Bytes.set current i (Char.chr h) ;
        aux (i+1)
      )
    with End_of_file -> ()
  in
  aux 0 ;
  let oc = open_out_gen [Open_wronly;Open_binary] 0 filename in
  Save.update_box_names section current ;
  Save.write_section oc addr section ;
  close_out oc ;
  Format.printf "Save has been successfully modified."
