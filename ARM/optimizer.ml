open Int32
open Utils
open Arm

exception CannotOptimize

type tweaking_settings =
  | NoTweaking
  | TweakFixedLength of int
  | TweakMinLength

module UInt32 = struct
  type t = int32
  (*let equal = equal*)
  let compare = unsigned_compare
end

module UInt32Set = Set.Make(UInt32)

let padding_code = Custom zero

let carry_out i = compare i zero < 0

let compute_all_constants _ =
  let rec all_immed8 max acc i =
    if equal i max then i::acc
    else all_immed8 max (i::acc) (succ i)
  in
  let immed8 = all_immed8 (of_int 255) [] zero in
  let immed8 = immed8 |> List.filter (fun i ->
    Name.is_code_writable [Utils.uint32_to_int i]
  ) in
  let rec all_rotations initial acc i =
    let i = Arm.rotate_right i |> Arm.rotate_right in
    if equal i initial then acc
    else all_rotations initial (i::acc) i
  in
  immed8 |> List.map (fun i ->
    all_rotations i [i] i
  ) |> List.flatten |> UInt32Set.of_list

let constants_set = compute_all_constants ()
let constants_set_no_carry =
  constants_set |> UInt32Set.filter (fun i -> carry_out i |> not)

let constants = constants_set |> UInt32Set.elements
let rev_constants = List.rev constants

let constants_mov_mvn =
  let nset = UInt32Set.map lognot constants_set in
  UInt32Set.union constants_set_no_carry nset
  |> UInt32Set.remove Int32.zero (* We try to avoid setting the zero flag *)
  |> UInt32Set.elements

let constants_mov_mvn_strict =
  let nset = UInt32Set.map lognot constants_set in
  UInt32Set.union constants_set nset
  |> UInt32Set.elements

let rev_constants_mov_mvn = List.rev constants_mov_mvn
let rev_constants_mov_mvn_strict = List.rev constants_mov_mvn_strict

let tries_at_depth_0 = [| 0x10000 ; 0x10000 ; 0x10000 ; 0x1000 ; 0x100 ; 0x10 ; 0x1 |]

let rec remove_while f lst =
  match lst with
  | [] -> []
  | i'::lst when f i' -> remove_while f lst
  | lst -> lst

type constants_cat = Arith | MovMvn | MovMvnStrict
let synthesis ~constants_cat ~additive ~incr max_card i is_valid_fst is_valid =
  let tad0 = tries_at_depth_0 in
  let tad0_len = Array.length tad0 in
  let tad0 = if tad0_len < max_card then tad0.(tad0_len-1) else tad0.(max_card-1) in

  let remove = (fun i -> remove_while (fun j -> unsigned_compare i j < 0)) in
  let rec next acc rc i =
    let rec aux try_nb rc =
      if equal i zero then Some acc
      else
        let depth = List.length acc in
        if depth >= max_card then None
        else
          let rem_depth = max_card - depth |> Int64.of_int in
          let i64 = int64_of_uint32 i in
          let ii = if incr then pred i else i in
          match remove ii rc with
          | [] -> None
          | fst::_ when Int64.unsigned_compare (* Optimisation *)
                          (Int64.mul ((if incr then succ fst else fst)
                          |> int64_of_uint32) rem_depth) i64 < 0 -> None
          | fst::rc ->
            let remainder = sub ii fst in
            begin match next (fst::acc) (fst::rc) remainder with
            | None -> aux (try_nb+1) rc
            | Some res -> Some res
            end
    in
    aux 0 rc
  in

  let filtered_rev_constants = List.filter is_valid rev_constants in
  let remove_init =
    if additive then remove
    else (fun i -> remove_while (fun j -> unsigned_compare i j > 0))
  in
  let op_init = if additive then sub else (fun x y -> sub y x) in
  let rec init try_nb rc =
    if try_nb >= tad0 then None
    else match remove_init i rc with
    | [] -> None
    | fst::rc ->
      let remainder = op_init i fst in
      begin match next [fst] filtered_rev_constants remainder with
      | None -> init (try_nb+1) rc
      | Some res -> Some res
      end
  in

  let init_rc =
    match additive, constants_cat with
    | true, Arith -> rev_constants
    | true, MovMvn -> rev_constants_mov_mvn
    | true, MovMvnStrict -> rev_constants_mov_mvn_strict
    | false, Arith -> constants
    | false, MovMvn -> constants_mov_mvn
    | false, MovMvnStrict -> constants_mov_mvn_strict
  in
  let init_rc = List.filter is_valid_fst init_rc in
  init 0 init_rc |>
  (function None -> None | Some lst -> Some (List.rev lst))

let synthesis_optimal ~constants_cat ~incr_add ~incr_sub max_card i is_valid_fst is_valid =
  let rec aux card =
    (*Format.printf "Trying with card=%i@." card ;*)
    if card > max_card then None
    else
      match synthesis ~constants_cat ~additive:true ~incr:incr_add
                  card i is_valid_fst (is_valid true) with
      | Some lst -> Some (lst, true)
      | None ->
        begin match synthesis ~constants_cat ~additive:false ~incr:incr_sub
                        card i is_valid_fst (is_valid false) with
        | Some lst -> Some (lst, false)
        | None -> aux (card+1)
        end
  in
  aux 1

let synthesis_test max_card i =
  synthesis_optimal ~constants_cat:MovMvn ~incr_add:false ~incr_sub:true
    max_card i (fun _ -> true) (fun _ _ -> true)

let is_command_valid arm =
  try (
    arm_to_binary arm |>
    List.exists (fun i -> Name.codes_for_command i |> Name.is_code_writable)
  ) with InvalidCommand -> false

let tweak_mov_mvn strict instr s cond rd rs max_card =
  let cmd = Mov {instr;s;cond;rd;rs} in
  match rs with
  | Register _ -> [cmd]
  | ScaledRegister _ -> failwith "Not implemented"
  | Immediate i ->
    let mk_cmd_first fst =
      let nfst = lognot fst in
      let constant_set_mov = if strict then constants_set else constants_set_no_carry in
      let is_mov =
        (instr = MOV && UInt32Set.mem fst constant_set_mov)
        || (UInt32Set.mem nfst constants_set |> not)
      in
      match is_mov, strict with
      | true, false -> Mov {instr=MOV;s=true;cond;rd;rs=Immediate fst}
      | true, true -> Mov {instr=MOV;s;cond;rd;rs=Immediate fst}
      | false, false -> Mov {instr=MVN;s=false;cond;rd;rs=Immediate nfst}
      | false, true -> Mov {instr=MVN;s;cond;rd;rs=Immediate nfst}
    in
    let mk_cmd additive i =
      match additive, strict with
      | true, false -> DataProc {instr=ADC;s=(rd=15 || rd=0);cond;rd;rn=rd;op2=Immediate i}
      | true, true -> DataProc {instr=ADD;s;cond;rd;rn=rd;op2=Immediate i}
      | false, false -> DataProc {instr=SBC;s=false;cond;rd;rn=rd;op2=Immediate i}
      | false, true -> DataProc {instr=SUB;s;cond;rd;rn=rd;op2=Immediate i}
    in
    let i = if instr = MOV then i else lognot i in
    begin match synthesis_optimal ~constants_cat:(if strict then MovMvnStrict else MovMvn)
                  ~incr_add:false ~incr_sub:(not strict) max_card i
                  (fun i -> mk_cmd_first i |> is_command_valid)
                  (fun add i -> mk_cmd add i |> is_command_valid) with
    | None -> [cmd]
    | Some (fst::lst, additive) ->
      (mk_cmd_first fst)::(List.map (mk_cmd additive) lst)
    | _ -> assert false
    end

let tweak_arith strict instr s cond rd rn op2 max_card =
  assert (instr = ADC || instr = SBC || instr = ADD || instr = SUB) ;
  let cmd = DataProc {instr;s;cond;rd;rn;op2} in
  match op2 with
  | Register _ -> [cmd]
  | ScaledRegister _ -> failwith "Not implemented"
  | Immediate i ->
    let i = if not strict && instr = SUB then pred i else i in (* Because the SUB will be replaced by SBC *)
    let is_addition =  instr = ADC || instr = ADD in
    let mk_cmd_first fst =
      match is_addition, strict with
      | _, true -> DataProc {instr;s;cond;rd;rn;op2=Immediate fst}
      | true, false -> DataProc {instr=ADC;s=(rn=15 || rn=0);cond;rd;rn;op2=Immediate fst}
      | false, false -> DataProc {instr=SBC;s=false;cond;rd;rn;op2=Immediate fst}
    in
    let mk_cmd additive i =
      let must_add = (additive && is_addition) || (not additive && not is_addition) in
      match must_add, strict with
      | true, false -> DataProc {instr=ADC;s=(rd=15 || rd=0);cond;rd;rn=rd;op2=Immediate i}
      | true, true -> DataProc {instr=ADD;s;cond;rd;rn=rd;op2=Immediate i}
      | false, false -> DataProc {instr=SBC;s=false;cond;rd;rn=rd;op2=Immediate i}
      | false, true -> DataProc {instr=SUB;s;cond;rd;rn=rd;op2=Immediate i}
    in
    begin match synthesis_optimal ~constants_cat:Arith ~incr_add:(not is_addition && not strict)
                  ~incr_sub:(is_addition && not strict) max_card i
                  (fun i -> mk_cmd_first i |> is_command_valid)
                  (fun add i -> mk_cmd add i |> is_command_valid) with
    | None -> [cmd]
    | Some (fst::lst, additive) ->
      (mk_cmd_first fst)::(List.map (mk_cmd additive) lst)
    | _ -> assert false
    end

let tweak_command (arm, optimize) =
  let strict =
    match !Settings.tweaker_mode with
    | Settings.Flexible -> false | Settings.Strict -> true
  in
  let optimize_with_card arm n pad =
    let res =
      match arm with
      | Mov {instr;s;cond;rd;rs} -> tweak_mov_mvn strict instr s cond rd rs n
      | DataProc {instr;s;cond;rd;rn;op2}
      when instr = ADC || instr = SBC || instr = ADD || instr = SUB ->
        tweak_arith strict instr s cond rd rn op2 n
      | _ -> [arm]
    in
    if pad
    then
      let padding = List.init (n - (List.length res)) (fun _ -> padding_code) in
      res@padding
    else res
  in
  match optimize with
  | NoTweaking -> [arm]
  | TweakMinLength -> optimize_with_card arm 5 false
  | TweakFixedLength card -> optimize_with_card arm card true

let tweak_arm lst =
  lst |> List.map tweak_command |> List.flatten

let do_not_tweak_arm lst =
  lst |> List.map (fun (arm, optimize) ->
    if optimize <> NoTweaking then raise CannotOptimize else arm
  )
