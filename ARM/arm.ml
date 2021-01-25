
type sign = int

type register = int

type conditional = EQ | NE | CS | HS | CC | LO | MI | PL 
                 | VS | VC | HI | LS | GE | LT | GT | LE | AL
type ldr_str_type = B | SB | H | SH | W

type operand = Constant of int | Register of register (* Register with shift is not supported for now *)
type ext_operand = Operand of operand | Imm16 of int
type scale_type = LSL of int | LSR of int | ASR of int | ROR of int | RRX
type register_offset = Immediate of register * int | Register of register * sign * register | ScaledRegister of register * sign * register * scale_type

type arm =
  | LDR of { typ: ldr_str_type ; cond: conditional ; rd: register ; rn: register_offset }
  | STR of { typ: ldr_str_type ; cond: conditional ; rd: register ; rn: register_offset }

  | MOV of { s:bool ; cond: conditional ; rd: register ; rs: ext_operand }
  | MVN of { s:bool ; cond: conditional ; rd: register ; rs: operand }

  | ADC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }
  | SBC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }

  | BIC of { s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }

  | BX of { cond: conditional ; rm: register }

open Int32

exception Invalid

let pc = 15
let sp = 13

let sign_plus = 1
let sign_minus = 0

let int4 = 0b1111
let mask4 = int4 |> of_int
let int8 = 0b11111111
let mask8 = int8 |> of_int
let int12 = 0b111111111111
let mask12 = int12 |> of_int

let sign_of i = if i >= 0 then (sign_plus, i) else (sign_minus, -i)

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
  | Immediate (r, _) | Register (r, _, _) | ScaledRegister (r, _, _, _) -> r

let addr_mode_2 ro = (* Load and store of word and ubyte *)
  let (sign, reg, v) =
    match ro with
    | Immediate (_, o) ->
      let (sign, o) = sign_of o in
      if o > int12 then raise Invalid ;
      (sign, 0, of_int o)
    | Register (_, sign, rm) ->
      (sign, 1, of_int rm)
    | ScaledRegister _ -> failwith "Not implemented"
  in
  let i = shift_left (of_int reg) 25 in
  let u = shift_left (of_int sign) 23 in
  let p = shift_left (of_int 1) 24 in
  let w = shift_left (of_int 0) 21 in
  logor v u |> logor p |> logor w |> logor i

let addr_mode_3 ro = (* Other load and store *)
  let (sign, imm, v) =
    match ro with
    | Immediate (_, o) ->
      let (sign, o) = sign_of o in
      if o > int8 then raise Invalid ;
      let v = of_int o in
      let immedL = logand mask4 v in
      let immedH = logand mask4 (shift_right_logical v 4) in
      (sign, 1, logor immedL (shift_left immedH 8))
    | Register (_, sign, rm) ->
      (sign, 0, of_int rm)
    | ScaledRegister _ -> raise Invalid
  in
  let i = shift_left (of_int imm) 22 in
  let u = shift_left (of_int sign) 23 in
  let p = shift_left (of_int 1) 24 in
  let w = shift_left (of_int 0) 21 in
  logor v u |> logor p |> logor w |> logor i

let ldr_str_to_binary is_ldr typ cond rd rn =
  let opcode = match is_ldr, typ with
  | true, B  -> 0b0100_0101_0000_0000_0000_0000_0000
  | true, SB -> 0b0000_0001_0000_0000_0000_1101_0000
  | true, H  -> 0b0000_0001_0000_0000_0000_1011_0000
  | true, SH -> 0b0000_0001_0000_0000_0000_1111_0000
  | true, W  -> 0b0100_0001_0000_0000_0000_0000_0000
  | false, B  -> 0b0100_0100_0000_0000_0000_0000_0000
  | false, SB -> raise Invalid
  | false, H  -> 0b0000_0000_0000_0000_0000_1011_0000
  | false, SH -> raise Invalid
  | false, W  -> 0b0100_0000_0000_0000_0000_0000_0000
  in
  let v = of_int opcode in
  let v = v |>
    add_condition_code cond |>
    add_rn_code (register_of_register_offset rn) |>
    add_rd_code rd in
  let addr_mode =
    match typ with
    | B | W -> addr_mode_2 rn
    | H | SH | SB -> addr_mode_3 rn
  in
  logor v addr_mode

let arm_to_binary arm =
  match arm with
  | LDR {typ;cond;rd;rn} -> ldr_str_to_binary true typ cond rd rn
  | STR {typ;cond;rd;rn} -> ldr_str_to_binary false typ cond rd rn
  | _ -> failwith "TODO"
