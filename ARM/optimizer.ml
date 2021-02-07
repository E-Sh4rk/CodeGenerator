open Int32
open Arm

module UInt32 = struct
  type t = int32
  (*let equal = equal*)
  let compare = unsigned_compare
end

module UInt32Set = Set.Make(UInt32)

let compute_all_constants _ =
  let rec all_immed8 max acc i =
    if equal i max then i::acc
    else all_immed8 max (i::acc) (succ i)
  in
  let immed8 = all_immed8 (of_int 255) [] zero in
  let immed8 = immed8 |> List.filter (fun i ->
    Name.is_code_writable [Name.int32_to_int i]
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
let constants = constants_set |> UInt32Set.elements
let rev_constants = List.rev constants

let constants_and_neg =
  let nset = UInt32Set.map neg constants_set in
  UInt32Set.union constants_set nset |> UInt32Set.elements

let rev_constants_and_neg = List.rev constants_and_neg

let tries_at_depth_0 = 200

let rec remove_while f lst =
  match lst with
  | [] -> []
  | i'::lst when f i' -> remove_while f lst
  | lst -> lst

let synthesis ~allow_mvn ~additive max_card i =
  let tad0 = tries_at_depth_0 in
  let tred = if max_card > 1 then tad0 / (max_card-1) else tad0 in

  let remove = (fun i -> remove_while (fun j -> unsigned_compare i j < 0)) in
  let rec aux try_nb acc rc i =
    if equal i zero then Some acc
    else
      let depth = List.length acc in
      if depth >= max_card then None
      else match remove i rc with
      | [] -> None
      | fst::rc ->
        let remainder = sub i fst in
        begin match aux 0 (fst::acc) (fst::rc) remainder with
        | None ->
          if try_nb < (tad0-tred*depth)
          then aux (try_nb+1) acc rc i
          else None
        | Some res -> Some res
        end
  in

  let remove_init =
    if additive then remove
    else (fun i -> remove_while (fun j -> unsigned_compare i j > 0))
  in
  let op_init = if additive then sub else (fun x y -> sub y x) in
  let rec init try_nb rc i =
    match remove_init i rc with
    | [] -> None
    | fst::rc ->
      let remainder = op_init i fst in
      begin match aux 0 [fst] rev_constants remainder with
      | None ->
        if try_nb < tad0
        then init (try_nb+1) rc i
        else None
      | Some res -> Some res
      end
  in

  let init_rc =
    if additive
    then (if allow_mvn then rev_constants_and_neg else rev_constants)
    else (if allow_mvn then constants_and_neg else constants)
  in
  init 0 init_rc i |>
  (function None -> None | Some lst -> Some (List.rev lst))

let synthesis_optimal ~allow_mvn max_card i =
  let rec aux card =
    if card > max_card then None
    else match synthesis ~allow_mvn ~additive:true card i with
    | Some lst -> Some (lst, true)
    | None ->
      begin match synthesis ~allow_mvn ~additive:false card i with
      | Some lst -> Some (lst, false)
      | None -> aux (card+1)
      end
  in
  aux 1

let fix_mov_or_mvn is_mov s cond rd rs =
  let cmd = if is_mov then MOV {s;cond;rd;rs} else MVN {s;cond;rd;rs} in
  match rs with
  | Register _ -> [cmd]
  | ScaledRegister _ -> failwith "Not implemented"
  | Immediate i ->
    let i = if is_mov then i else neg i in
    begin match synthesis_optimal ~allow_mvn:true 5 i with
    | None -> [cmd]
    | Some (fst::lst, additive) ->
      let nfst = neg fst in
      let is_mov =
        (is_mov && UInt32Set.mem fst constants_set) ||
        (UInt32Set.mem nfst constants_set |> not)
      in
      let fcmd =
        if is_mov
        then MOV {s=true;cond;rd;rs=Immediate fst}
        else MVN {s=false;cond;rd;rs=Immediate nfst}
      in
      let cmds = lst |> List.map (fun i ->
        if additive
        then ADC {s=false;cond;rd;rn=rd;op2=Immediate i}
        else SBC {s=false;cond;rd;rn=rd;op2=Immediate i}
      ) in
      fcmd::cmds
    | _ -> assert false
    end

let fix_adc_or_sbc is_adc s cond rd rn op2 =
  let cmd = if is_adc then ADC {s;cond;rd;rn;op2} else SBC {s;cond;rd;rn;op2} in
  match op2 with
  | Register _ -> [cmd]
  | ScaledRegister _ -> failwith "Not implemented"
  | Immediate i ->
    begin match synthesis_optimal ~allow_mvn:false 5 i with
    | None -> [cmd]
    | Some (fst::lst, additive) ->
      let fcmd =
        if is_adc
        then ADC {s;cond;rd;rn;op2=Immediate fst}
        else SBC {s;cond;rd;rn;op2=Immediate fst}
      in
      let cmds = lst |> List.map (fun i ->
        if (additive && is_adc) || (not additive && not is_adc)
        then ADC {s=false;cond;rd;rn=rd;op2=Immediate i}
        else SBC {s=false;cond;rd;rn=rd;op2=Immediate i}
      ) in
      fcmd::cmds
    | _ -> assert false
    end

let fix_command arm =
  match arm with
  | MOV {s;cond;rd;rs} -> fix_mov_or_mvn true s cond rd rs
  | MVN {s;cond;rd;rs} -> fix_mov_or_mvn false s cond rd rs
  | ADC {s;cond;rd;rn;op2} -> fix_adc_or_sbc true s cond rd rn op2
  | SBC {s;cond;rd;rn;op2} -> fix_adc_or_sbc false s cond rd rn op2
  | _ -> [arm]

let fix_arm lst =
  lst |> List.map (fun (arm, optimize) ->
    if optimize then fix_command arm else [arm]
  ) |> List.flatten

exception CannotOptimize

let do_not_fix_arm lst =
  lst |> List.map (fun (arm, optimize) ->
    if optimize then raise CannotOptimize else arm
  )
