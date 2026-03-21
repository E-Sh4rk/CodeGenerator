open Utils

let mult_mod x y m =
  Z.rem (Z.mul x y) m

let pred = Z.pred
let succ = Z.succ
let int_mul i bi = Z.mul (Z.of_int i) bi
let logand = Z.logand
let shift_left = Z.shift_left
let shift_right = Z.shift_right
let two = Z.of_int 2
let three = Z.of_int 3
let four = Z.of_int 4

let a = 0x41c64e6dL |> Z.of_int64
let b = 0x6073L |> Z.of_int64
let a_inv = 0xEEB9EB65L |> Z.of_int64
let b_inv = 0x0A3561A1L |> Z.of_int64
let m = 0x1_00_00_00_00L |> Z.of_int64
let b_1 = 0x341b944bbL |> Z.of_int64 (* Inverse of b mod 2**34 *)
let cycle_part_product =
  mult_mod (pred a) b_1 (int_mul 4 m)

let mask32 = Z.of_int64 0xFFFFFFFFL

let even n = logand n Z.one |> Z.equal Z.zero
let odd n = even n |> not

let rec mpow base exp n =
  let base = Z.rem base n in
  if Z.equal exp Z.zero
  then Z.one
  else if Z.equal exp Z.one
  then base
  else if even exp
  then mpow (Z.mul base base) (Z.div exp two) n
  else mult_mod base (mpow base (pred exp) n) n

let rec pow base exp =
  if Z.equal exp Z.zero
  then Z.one
  else if Z.equal exp Z.one
  then base
  else if even exp
  then pow (Z.mul base base) (Z.div exp two)
  else Z.mul base (pow base (pred exp))

let add_if_negative x y =
  if Z.geq x Z.zero then x else Z.add x y
let sub_if_geq x y =
  if Z.geq x y then Z.sub x y else x

let seed_at cycle =
  let res = Z.mul (pred a) m in
  let op1 = add_if_negative (mpow a cycle res |> pred) res in
  let aux = Z.div op1 (pred a) in
  mult_mod aux b m

exception DoesNotExist

let discrete_log base power n check_exists =
  let (a,c,m) = (base, power, pow two (Z.of_int n)) in
  assert (n >= 3) ;
  assert (odd a && odd c) ;
  if check_exists
  then begin
    let mod_switch = Z.equal (Z.rem a four) three in
    let rec aux k m1 =
      if k >= 2 then begin
        let x = if mod_switch then mult_mod a a m1 else Z.rem a m1 in
        if Z.equal x Z.one then begin
          let x = Z.rem c m1 in
          if ((Z.equal x Z.one) || (mod_switch && Z.equal x a)) |> not
          then raise DoesNotExist
        end else aux (k-1) (shift_right m1 1)
      end
    in
    aux (n-1) (shift_right m 1)
  end ;
  let k = n-2 in
  let bitmask = pow two (k-1 |> Z.of_int) |> pred in
  let ls = Array.make k c in
  let rec aux i l =
    if i < k
    then begin
      let l = mult_mod l l m in
        ls.(i) <- l ; aux (i+1) l
    end
  in
  aux 1 c ;
  let rec aux i b bit =
    if i >= 0
    then begin
      let b =
        if Z.equal ls.(i) (mpow a (shift_left b i |> logand bitmask) m) |> not
        then Z.add b bit
        else b
      in
      aux (i-1) b (shift_left bit 1)
    end else b
  in
  aux (k-1) Z.zero Z.one

let cycle_to seed =
  let modulo = int_mul 4 m in
  let power = sub_if_geq (mult_mod seed cycle_part_product modulo |> succ) modulo in
  discrete_log a power (32+2) false

let next_seed seed =
  Z.mul seed a |> Z.add b |> logand mask32

let prev_seed seed =
  Z.mul seed a_inv |> Z.add b_inv |> logand mask32

let big_int_of_uint32 i32 =
  int64_of_uint32 i32
  |> Z.of_int64

let uint32_of_bigint bi =
  let maxint32 = Int32.max_int |> Z.of_int32 in
  let minint32 = Int32.min_int |> Z.of_int32 in
  let total = Z.sub maxint32 minint32 |> Z.succ in
  let bi = if Z.gt bi maxint32
    then Z.sub bi total
    else bi
  in
  Z.to_int32 bi

(* Fishing (from Shao) *)

let rng_of seed = shift_right seed 16 |> Z.to_int

let old_rod = 0
let good_rod = 1
let super_rod = 2
let aPresses = [|1;3;6|]
let moreAPresses = [|0;10;30|]
let fishing_attempt route119 feebas starting_seed rod initialAdvances =
  let seed = ref starting_seed in
  for _=1 to initialAdvances do
    seed := next_seed !seed
  done ;
  let minRounds = ((rng_of !seed) mod (aPresses.(rod))) + 1 in
  seed := next_seed !seed ; (* One call to determine number of dots on first cast *)
  seed := next_seed !seed ; (* call that determines if fish is on line *)

  let biteRoll = (rng_of !seed) mod 100 in
  let biteResult =
    if biteRoll land 1 = 0 then 0 (* A fish will be generated regardless of lead *)
    else if biteRoll > 14 then 1 (* A Suction Cups or Sticky Hold lead is necessary to get a fish *)
    else 2
  in

  for _=1 to minRounds-1 do
    seed := next_seed !seed (* More calls to determine dot numbers *)
  done ;

  let advancement = ref (initialAdvances + minRounds) in
  if minRounds = 1 then begin
    seed := next_seed !seed ; (* Decide whether or not to make the player reel again *)
    advancement := !advancement + 1 ;
    let biteRoll = (rng_of !seed) mod 100 in
    if biteRoll < moreAPresses.(rod)
    then (seed := next_seed !seed ;  advancement := !advancement + 1)
  end ;
  seed := next_seed !seed ;
  let feebasResult = (rng_of !seed) mod 100 < 50 in
  if route119 && not feebas then advancement := !advancement + 1 ;
  (biteResult, feebasResult, !advancement)

let best_seed_for_rod route119 feebas target_seed rod =
  let maxAdditionalOffsets = [|0;1;4|] in
  let start = 7 in
  let bestResults = ref None in

  let starting_seed = ref (prev_seed target_seed) in
  for _=1 to start + 1 do
    starting_seed := prev_seed !starting_seed
  done ;

  let offset119 =
    if route119 && not feebas
    then (starting_seed := prev_seed !starting_seed ; 1)
    else 0
  in

  for additionalOffset=0 to maxAdditionalOffsets.(rod) do
    let (biteResult, feebasResult, advancement) =
      fishing_attempt route119 feebas !starting_seed rod start in
    starting_seed := prev_seed !starting_seed ;
    if biteResult < 2 && (not feebas || feebasResult)
      && start + 2 + additionalOffset + offset119 = advancement
    then begin
      bestResults :=
        match !bestResults with
        | None -> Some (biteResult, advancement, next_seed !starting_seed)
        | Some (br, adv, s) when br = 0 && biteResult = 1 -> Some (br, adv, s)
        | Some (br, adv, s) when br = biteResult && adv < advancement -> Some (br, adv, s)
        | _ -> Some (biteResult, advancement, next_seed !starting_seed)
    end
  done ;
  !bestResults

(* Interface *)

let next_seed seed =
  next_seed (big_int_of_uint32 seed) |> uint32_of_bigint

let prev_seed seed =
  prev_seed (big_int_of_uint32 seed) |> uint32_of_bigint

let cycle_to seed =
  cycle_to (big_int_of_uint32 seed) |> uint32_of_bigint

let seed_at cycle =
  seed_at (big_int_of_uint32 cycle) |> uint32_of_bigint

let best_seed_for_rod route119 feebas target_seed rod =
  match best_seed_for_rod route119 feebas (big_int_of_uint32 target_seed) rod with
  | None -> None
  | Some (b,i,s) -> Some (b=1,i,uint32_of_bigint s)
