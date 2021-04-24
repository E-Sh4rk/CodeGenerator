
let enumerate_files dirname ext =
  Sys.readdir dirname
  |> Array.to_list
  |> List.filter (fun x -> Filename.extension x = ext)

let uint32_of_str str =
  let str = String.lowercase_ascii str in
  (* Issue with js_of_ocaml... *)
  (*
  if Str.string_match (Str.regexp "[0-9]+$") str 0
  then Int32.of_string ("0u"^str)
  else Int32.of_string str
  *)
  let i64 = Int64.of_string str in
  if Int64.logand 0xFFFFFFFF00000000L i64 |> Int64.equal Int64.zero
  then Int64.to_int32 i64
  else raise (Failure "Not a valid int32.")

let uint32_to_int v =
  match Int32.unsigned_to_int v with None -> assert false | Some i -> i

let int64_of_uint32 x =
  Int64.of_int32 x |> Int64.logand 0xFFFFFFFFL

let dummy_fmt =
  Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let concat_strings lst =
  let pp fmt lst =
    lst |> List.iter (fun str -> Format.fprintf fmt "%s" str)
  in
  Format.asprintf "%a" pp lst