
module UString = Uutf.String
module UBuffer = Uutf.Buffer

let string_of_uchar uchar =
  let buffer = Buffer.create 4 in
  UBuffer.add_utf_8 buffer uchar ;
  Buffer.contents buffer

let decompose_into_uchars str =
  UString.fold_utf_8 (fun acc _ uc -> match uc with
  | `Malformed _ -> raise Not_found
  | `Uchar uc -> uc::acc) [] str
  |> List.rev

type result = 
  | Cont of (Format.formatter -> string -> result)
  | NoCont

let rec main fmt =
  Format.fprintf fmt "Available characters: @." ;
  Charset.all_writable_chars |>
  List.iter (Format.fprintf fmt "%s") ;
  Format.fprintf fmt "@.Please enter text to encode: @?" ;
  Cont main_1

and main_1 fmt str =
  let rec aux2 lst =
    match lst with
    | [] -> ()
    | a::b::lst ->
      Format.fprintf fmt "%02X%02X " b a ;
      aux2 lst
    | _ -> assert false
  in
  let rec aux4 lst =
    match lst with
    | [] -> ()
    | a::b::c::d::lst ->
      Format.fprintf fmt "%02X%02X%02X%02X " d c b a ;
      aux4 lst
    | _ -> assert false
  in
  begin try
    let encoding =
      decompose_into_uchars str |>
      List.map string_of_uchar |>
      List.map Charset.encode_writable_char in
    let encoding = encoding@[Name.eof] in

    Format.fprintf fmt "Encoded data (1-byte): @." ;
    List.iter (Format.fprintf fmt "%02X ") encoding ;

    let n = List.length encoding in
    let encoding = if n mod 2 <> 0 then encoding@[0] else encoding in
    Format.fprintf fmt "@.Encoded data (2-bytes): @." ;
    aux2 encoding ;

    let n = List.length encoding in
    let encoding = if n mod 4 <> 0 then encoding@[0;0] else encoding in
    Format.fprintf fmt "@.Encoded data (4-bytes): @." ;
    aux4 encoding ;
    Format.fprintf fmt "@."
  with Not_found ->
    Format.fprintf fmt "An error occured. Please check you only use available characters."
  end ;
  NoCont
