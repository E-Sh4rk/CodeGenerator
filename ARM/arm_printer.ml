
let pp_arm _ _ = ()

let int32_to_int i =
  match Int32.unsigned_to_int i with
  | None -> failwith "Only 64bits machines are supported"
  | Some i -> i

let pp_hex fmt i =
  Format.fprintf fmt "%08X" (int32_to_int i)
