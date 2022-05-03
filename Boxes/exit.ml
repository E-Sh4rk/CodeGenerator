
type t = ((int * ((int list * (int32 * Arm.arm)) list)) list) *
         (((int list * (int32 * Arm.arm)) list) option)

let assemble_arm arm =
  let code =
    Arm.arm_to_binary arm |>
    List.map Name.codes_for_command |>
    Name.preferred_code
  in
  let hex = Name.command_for_codes code in
  (code, (hex, arm))

let load_from_dir env dirname =
  try (
    Utils.enumerate_files dirname ".txt"
    |> List.map (fun x ->
      let path = Filename.concat dirname x in
      let str = Filename.basename x |> Filename.remove_extension in
      let i = int_of_string str in
      let arm = Parse.from_filename ~headers:false path |>
        Parse.parsed_content_to_arm Utils.dummy_fmt ~optimize:false env in
      let codes = arm |> List.map assemble_arm
      in
      (i, codes)
    )
    |> List.sort (fun (i,_) (j,_) -> compare i j)
    |> (fun x -> (x, None))
  )
  with Optimizer.CannotOptimize ->
    failwith "Exit codes cannot be tweaked (please remove interrogation marks)."

let load_from_parsed_file fmt env (h, arm) =
  let codes = (h, arm) |>
    Parse.parsed_content_to_arm fmt ~optimize:true env |>
    List.map assemble_arm
  in
  match Preprocess.get_param h "start" with
  | HInt i -> ([(Utils.uint32_to_int i, codes)], None)
  | HNone -> ([], Some codes)
  | _ -> failwith "Exit code has invalid headers."

let load_from_file fmt env filename =
  let (headers,ast) = Parse.from_filename ~headers:true filename in
  (headers, load_from_parsed_file fmt env (headers, ast))

exception NoExitCode

let get_preferred (lst, default) i =
  try (
    let rec aux lst =
      match lst with
      | [] -> raise NoExitCode
      | (j, c)::_ when i <= j -> (j, c)
      | _::lst -> aux lst
    in
    aux lst
  ) with NoExitCode -> (
    match default with
    | None -> raise NoExitCode
    | Some c -> (i, c)
  )

let get_preferred_raw a b =
  let (i,lst) = get_preferred a b in
  (i, List.map fst lst)

let get_preferred_descr a b =
  let (i,lst) = get_preferred a b in
  (i, List.map snd lst)
