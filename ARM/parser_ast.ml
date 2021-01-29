open Arm

type offset =
  | OImmediate of Arm.sign * int32
  | ORegister of Arm.sign * string
  | ONone

type args =
  | Register of string
  | Immediate of int32
  | Offset of string (* register *) * offset * bool (* pre-indexed *)

type command =
  Lexing.position * string * args list

type ast = command list

let int32_of_str str = String.lowercase_ascii str |> Int32.of_string

exception StructError

let recognize_condition str i =
  let n = String.length str in
  let str = String.sub str i (min 2 (n-i)) in
  match str with
  | "EQ" -> (Some EQ, i+2)
  | "NE" -> (Some NE, i+2)
  | "CS" -> (Some CS, i+2)
  | "HS" -> (Some HS, i+2)
  | "CC" -> (Some CC, i+2)
  | "LO" -> (Some LO, i+2)
  | "MI" -> (Some MI, i+2)
  | "PL" -> (Some PL, i+2)
  | "VS" -> (Some VS, i+2)
  | "VC" -> (Some VC, i+2)
  | "HI" -> (Some HI, i+2)
  | "LS" -> (Some LS, i+2)
  | "GE" -> (Some GE, i+2)
  | "LT" -> (Some LT, i+2)
  | "GT" -> (Some GT, i+2)
  | "LE" -> (Some LE, i+2)
  | "AL" -> (Some AL, i+2)
  | _ -> (None, i)

let recognize_ldr_str_type str i =
  let n = String.length str in
  let str = String.sub str i (min 2 (n-i)) in
  match str with
  | "SB" -> (Some SB, i+2)
  | "SH" -> (Some SH, i+2)
  | _ ->
  begin
    let n = String.length str in
    let str = String.sub str i (min 1 (n-i)) in
    match str with
    | "B" -> (Some B, i+1)
    | "H" -> (Some H, i+1)
    | "W" -> (Some W, i+1)
    | _ -> (None, i)
  end

let recognize_s str i =
  let n = String.length str in
  let str = String.sub str i (min 1 (n-i)) in
  match str with
  | "S" -> (true, i+1)
  | _ -> (false, i)

let combine_modifiers (cond, lst, s) (cond', lst', s') =
  let cond = match cond, cond' with
  | None, None -> None
  | Some s, None | None, Some s -> Some s
  | Some _, Some _ -> raise StructError
  in
  let lst = match lst, lst' with
  | None, None -> None
  | Some s, None | None, Some s -> Some s
  | Some _, Some _ -> raise StructError
  in
  let s = match s, s' with
  | false, false -> false
  | true, false | false, true -> true
  | true, true -> raise StructError
  in
  (cond, lst, s)

let recognize_modifiers str i =
  let n = String.length str in
  let rec aux mods i =
    if i >= n then mods
    else
      let (nmods, i) =
        match recognize_condition str i with
        | (Some c, i) -> ((Some c, None, false), i)
        | (None, i) ->
          match recognize_ldr_str_type str i with
          | (Some lst, i) -> ((None, Some lst, false), i)
          | (None, i) ->
            match recognize_s str i with
            | (true, i) -> ((None, None, true), i)
            | _ -> raise StructError
      in
      let mods = combine_modifiers mods nmods in
      aux mods i
  in
  aux (None, None, false) i

let register_of_str str =
  let str = String.lowercase_ascii str in
  match str with
  | "r0" -> 0   | "r1" -> 1   | "r2" -> 2   | "r3" -> 3   | "r4" -> 4
  | "r5" -> 5   | "r6" -> 6   | "r7" -> 7   | "r8" -> 8   | "r9" -> 9 
  | "r10" -> 10 | "r11" -> 11 | "r12" -> 12 | "r13" -> 13 | "r14" -> 14 
  | "r15" -> 15 | "sb" -> sb  | "sl" -> sl  | "fp" -> fp  | "ip" -> ip
  | "sp" -> sp  | "lr" -> lr  | "pc" -> pc

let get_register arg =
  match arg with
  | Register str -> Arm.Register (register_of_str str)
  | _ -> raise StructError

let get_rd args = get_register (List.hd args)

let get_rn args =
  if List.length args > 2
  then get_rd (List.tail args)
  else get_rd args

let get_operand arg =
  match arg with
  | Immediate i -> Arm.Immediate i
  | Register str -> Arm.Register (register_of_str str)
  | _ -> raise StructError

let get_op2 args =
  let n = List.length args in
  get_operand (List.nth args (n-1))

let get_rs = get_op2

let get_ro args = failwith "TODO"

let cmd_to_arm (pos, cmd, args) =
  let cmd = String.uppercase_ascii cmd in
  let n = String.length cmd in
  if n < 3 then raise StructError ;
  
  let (cond, typ, s) = recognize_modifiers str 3 in
  let cond = match cond with None -> AL | Some c -> c in
  let typ = match typ with None -> W | Some typ -> typ in

  try match String.sub cmd 0 3 with
  | "LDR" -> LDR { typ ; cond ; rd=get_rd args ; ro=get_ro args }
  | "STR" -> STR { typ ; cond ; rd=get_rd args ; ro=get_ro args }
  | "MOV" -> MOV { s ; cond ; rd=get_rd args ; rs=get_rs args }
  | "MVN" -> MVN { s ; cond ; rd=get_rd args ; rs=get_rs args }
  | "ADC" -> ADC { s ; cond ; rd=get_rd args ; rn=get_rn args ; op2=get_op2 args }
  | "SBC" -> SBC { s ; cond ; rd=get_rd args ; rn=get_rn args ; op2=get_op2 args }
  | "BIC" -> BIC { s ; cond ; rd=get_rd args ; rn=get_rn args ; op2=get_op2 args }
  | _ -> raise StructError
  with Failure -> raise StructError

let to_arm ast =
  try Some (List.map cmd_to_arm ast) with StructError -> None