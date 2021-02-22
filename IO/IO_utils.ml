
let enumerate_files dirname ext =
  Sys.readdir dirname
  |> Array.to_list
  |> List.filter (fun x -> Filename.extension x = ext)
