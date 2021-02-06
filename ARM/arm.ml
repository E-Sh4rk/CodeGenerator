
type sign = int

type register = int

type conditional = EQ | NE | CS | HS | CC | LO | MI | PL 
                 | VS | VC | HI | LS | GE | LT | GT | LE | AL
type ldr_str_type = B | SB | H | SH | W | T | BT
type addressing_type = Offset | PreIndexed | PostIndexed

type scale_type = LSL of int | LSR of int | ASR of int | ROR of int | RRX
type operand = Immediate of int32 | Register of register | ScaledRegister of register * scale_type
type register_offset = OImmediate of register * sign * int32 | ORegister of register * sign * register | OScaledRegister of register * sign * register * scale_type

type arm =
  | Custom of int32

  | LDR of { typ: ldr_str_type ; cond: conditional ; rd: register ; ro: register_offset * addressing_type }
  | STR of { typ: ldr_str_type ; cond: conditional ; rd: register ; ro: register_offset * addressing_type }

  | MOV of { s:bool ; cond: conditional ; rd: register ; rs: operand }
  | MVN of { s:bool ; cond: conditional ; rd: register ; rs: operand }

  | ADC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }
  | SBC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }
  | BIC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }

open Int32

exception InvalidCommand

let a1 = 0
let a2 = 1
let a3 = 2
let a4 = 3
let v1 = 4
let v2 = 5
let v3 = 6
let v4 = 7
let v5 = 8
let v6 = 9
let v7 = 10
let v8 = 11
let sb = 9
let sl = 10
let fp = 11
let ip = 12
let sp = 13
let lr = 14
let pc = 15

let sign_plus = 1
let sign_minus = 0

let int1 = 0b1
let mask1 = int1 |> of_int
let int4 = 0b1111
let mask4 = int4 |> of_int
let int8 = 0b11111111
let mask8 = int8 |> of_int
let int12 = 0b111111111111
let mask12 = int12 |> of_int

let condition_code c =
  begin match c with
  | EQ -> 0b0000 | NE -> 0b0001 | CS | HS -> 0b0010 | CC | LO -> 0b0011
  | MI -> 0b0100 | PL -> 0b0101 | VS -> 0b0110 | VC -> 0b0111
  | HI -> 0b1000 | LS -> 0b1001 | GE -> 0b1010 | LT -> 0b1011 | GT -> 0b1100 | LE -> 0b1101 | AL -> 0b1110
  end
  |> of_int

let add_condition_code c v =
  shift_left (condition_code c) 28
  |> logor v

let add_rn_code rn v =
  shift_left (of_int rn) 16
  |> logor v

let add_rd_code rd v =
  shift_left (of_int rd) 12
  |> logor v

let register_of_register_offset ro =
  match ro with
  | OImmediate (r, _, _) | ORegister (r, _, _) | OScaledRegister (r, _, _, _) -> r

let rotate_right v =
  let lb = logand v mask1 in
  let v = shift_right_logical v 1 in
  logor v (shift_left lb 31)

let rotate_left v =
  let hb = logand v (shift_left mask1 31) in
  let v = shift_left v 1 in
  logor v (shift_right_logical hb 31)

let decompose_immediate imm =
  let rec aux n imm =
    if n > int4 then []
    else
      let others = aux (n+1) (rotate_left (rotate_left imm)) in
      let imm8 = logand imm mask8 in
      if equal imm8 imm then (n, imm8)::others else others
  in
  let res = aux 0 imm in
  if res = [] then raise InvalidCommand else res

let addr_mode_1 rs =
  let possibilities =
    match rs with
    | Immediate i ->
      decompose_immediate i
      |> List.map (fun (rr, imm8) ->
        (1, logor imm8 (shift_left (of_int rr) 8))
      )
    | Register (rm) ->
      [(0, of_int rm)]
    | ScaledRegister _ -> failwith "Not implemented"
  in
  possibilities |>
  List.map (fun (imm, v) ->
    let i = shift_left (of_int imm) 25 in
    logor v i
  )

let p_and_w addr_typ =
  match addr_typ with
  | Offset -> (1,0)
  | PreIndexed -> (1,1)
  | PostIndexed -> (0,0)

let addr_mode_2 ro addr_typ = (* Load and store of word and ubyte *)
  let (sign, reg, v) =
    match ro with
    | OImmediate (_, sign, v) ->
      if unsigned_compare v mask12 > 0 then raise InvalidCommand ;
      (sign, 0, v)
    | ORegister (_, sign, rm) ->
      (sign, 1, of_int rm)
    | OScaledRegister _ -> failwith "Not implemented"
  in
  let (p, w) = p_and_w addr_typ in
  let i = shift_left (of_int reg) 25 in
  let u = shift_left (of_int sign) 23 in
  let p = shift_left (of_int p) 24 in
  let w = shift_left (of_int w) 21 in
  logor v u |> logor p |> logor w |> logor i

let addr_mode_3 ro addr_typ = (* Other load and store *)
  let (sign, imm, v) =
    match ro with
    | OImmediate (_, sign, v) ->
      if unsigned_compare v mask8 > 0 then raise InvalidCommand ;
      let immedL = logand mask4 v in
      let immedH = logand mask4 (shift_right_logical v 4) in
      (sign, 1, logor immedL (shift_left immedH 8))
    | ORegister (_, sign, rm) ->
      (sign, 0, of_int rm)
    | OScaledRegister _ -> raise InvalidCommand
  in
  let (p, w) = p_and_w addr_typ in
  let i = shift_left (of_int imm) 22 in
  let u = shift_left (of_int sign) 23 in
  let p = shift_left (of_int p) 24 in
  let w = shift_left (of_int w) 21 in
  logor v u |> logor p |> logor w |> logor i

let ldr_str_to_binary is_ldr typ cond rd (rn, addr_typ) =
  let check_post_addr () =
    match addr_typ with
    | PostIndexed -> ()
    | _ -> raise InvalidCommand
  in
  let opcode = match is_ldr, typ with
  | true, B  -> 0b0100_0101_0000_0000_0000_0000_0000
  | true, SB -> 0b0000_0001_0000_0000_0000_1101_0000
  | true, H  -> 0b0000_0001_0000_0000_0000_1011_0000
  | true, SH -> 0b0000_0001_0000_0000_0000_1111_0000
  | true, W  -> 0b0100_0001_0000_0000_0000_0000_0000
  | true, T  -> check_post_addr () ; 0b0100_0011_0000_0000_0000_0000_0000
  | true, BT -> check_post_addr () ; 0b0100_0111_0000_0000_0000_0000_0000
  | false, B  -> 0b0100_0100_0000_0000_0000_0000_0000
  | false, SB -> raise InvalidCommand
  | false, H  -> 0b0000_0000_0000_0000_0000_1011_0000
  | false, SH -> raise InvalidCommand
  | false, W  -> 0b0100_0000_0000_0000_0000_0000_0000
  | false, T  -> check_post_addr () ; 0b0100_0010_0000_0000_0000_0000_0000
  | false, BT -> check_post_addr () ; 0b0100_0110_0000_0000_0000_0000_0000
  in
  let v = of_int opcode |>
    add_condition_code cond |>
    add_rn_code (register_of_register_offset rn) |>
    add_rd_code rd in
  let addr_mode =
    match typ with
    | B | W | T | BT -> addr_mode_2 rn addr_typ
    | H | SH | SB -> addr_mode_3 rn addr_typ
  in
  [logor v addr_mode]

let mov_mvn_to_binary is_mov s cond rd rs =
  let opcode = if is_mov
  then 0b0001_1010_0000_0000_0000_0000_0000
  else 0b0001_1110_0000_0000_0000_0000_0000 in
  let scode = if s then 1 else 0 in
  let scode = shift_left (of_int scode) 20 in
  let v = of_int opcode |>
    add_condition_code cond |>
    add_rd_code rd |>
    logor scode in
  addr_mode_1 rs |>
  List.map (fun addr_mode -> logor v addr_mode)


let calculation_to_binary typ s cond rd rn op2 =
  let opcode = match typ with
  | "adc" -> 0b0000_1010_0000_0000_0000_0000_0000
  | "sbc" -> 0b0000_1100_0000_0000_0000_0000_0000
  | "bic" -> 0b0001_1100_0000_0000_0000_0000_0000
  | _ -> assert false
  in
  let scode = if s then 1 else 0 in
  let scode = shift_left (of_int scode) 20 in
  let v = of_int opcode |>
    add_condition_code cond |>
    add_rd_code rd |>
    add_rn_code rn |>
    logor scode in
  addr_mode_1 op2 |>
  List.map (fun addr_mode -> logor v addr_mode)

let arm_to_binary arm =
  match arm with
  | Custom i -> [i]
  | LDR {typ;cond;rd;ro} -> ldr_str_to_binary true typ cond rd ro
  | STR {typ;cond;rd;ro} -> ldr_str_to_binary false typ cond rd ro
  | MOV {s;cond;rd;rs}   -> mov_mvn_to_binary true s cond rd rs
  | MVN {s;cond;rd;rs}   -> mov_mvn_to_binary false s cond rd rs
  | ADC {s;cond;rd;rn;op2} -> calculation_to_binary "adc" s cond rd rn op2
  | SBC {s;cond;rd;rn;op2} -> calculation_to_binary "sbc" s cond rd rn op2
  | BIC {s;cond;rd;rn;op2} -> calculation_to_binary "bic" s cond rd rn op2

let reverse_endianness v =
  let v1 = shift_left (logand mask8 v) (3*8) in
  let v = shift_right_logical v 8 in
  let v2 = shift_left (logand mask8 v) (2*8) in
  let v = shift_right_logical v 8 in
  let v3 = shift_left (logand mask8 v) (1*8) in
  let v = shift_right_logical v 8 in
  let v4 = logand mask8 v in
  logor v1 v2 |> logor v3 |> logor v4
