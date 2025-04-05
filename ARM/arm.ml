
type sign = int

type register = int

type conditional = EQ | NE | CS | HS | CC | LO | MI | PL 
                 | VS | VC | HI | LS | GE | LT | GT | LE | AL
type ldr_str_type = B | SB | H | SH | W | T | BT
type addressing_type = Offset | PreIndexed | PostIndexed

type scale_type = LSL of int | LSR of int | ASR of int | ROR of int | RRX
type operand = Immediate of int32 | Register of register | ScaledRegister of register * scale_type
type register_offset = OImmediate of register * sign * int32 | ORegister of register * sign * register | OScaledRegister of register * sign * register * scale_type

type data_proc_instr = ADC | SBC | BIC | AND (* for JP: *) | ADD | SUB | ORR | EOR
type mov_instr = MOV | MVN
type mem_instr = LDR | STR

type arm =
  | Custom of int32
  | Mem of { instr: mem_instr ; typ: ldr_str_type ; cond: conditional ; rd: register ; ro: register_offset * addressing_type }
  | Mov of { instr: mov_instr ; s:bool ; cond: conditional ; rd: register ; rs: operand }
  | DataProc of { instr: data_proc_instr ; s:bool ; cond: conditional ; rd: register ; rn: register ; op2: operand }
  | Branch of { l:bool ; cond: conditional ; target: int32 }
  | BranchX of { l:bool ; cond: conditional ; rm: register }
  (* BLX_imm is not supported yet *)

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
let int2 = 0b11
let mask2 = int2 |> of_int
let int4 = 0b1111
let mask4 = int4 |> of_int
let int8 = 0b11111111
let mask8 = int8 |> of_int
let int9 = 0b111111111
let mask9 = int9 |> of_int
let int12 = 0b111111111111
let mask12 = int12 |> of_int
let int24 = 0b111111111111_111111111111
let mask24 = int24 |> of_int

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

let add_rm_code rm v =
  of_int rm
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

let signed_immed24 i =
  if equal (logand i mask2) zero
  then
    let i = shift_right i 2 in
    let ms9 = shift_right_logical i 23 in
    if equal ms9 mask9 || equal ms9 zero
    then logand i mask24
    else raise InvalidCommand
  else raise InvalidCommand

let ldr_str_to_binary instr typ cond rd (rn, addr_typ) =
  let check_post_addr () =
    match addr_typ with
    | PostIndexed -> ()
    | _ -> raise InvalidCommand
  in
  let opcode = match instr, typ with
  | LDR, B  -> 0b0100_0101_0000_0000_0000_0000_0000
  | LDR, SB -> 0b0000_0001_0000_0000_0000_1101_0000
  | LDR, H  -> 0b0000_0001_0000_0000_0000_1011_0000
  | LDR, SH -> 0b0000_0001_0000_0000_0000_1111_0000
  | LDR, W  -> 0b0100_0001_0000_0000_0000_0000_0000
  | LDR, T  -> check_post_addr () ; 0b0100_0011_0000_0000_0000_0000_0000
  | LDR, BT -> check_post_addr () ; 0b0100_0111_0000_0000_0000_0000_0000
  | STR, B  -> 0b0100_0100_0000_0000_0000_0000_0000
  | STR, SB -> raise InvalidCommand
  | STR, H  -> 0b0000_0000_0000_0000_0000_1011_0000
  | STR, SH -> raise InvalidCommand
  | STR, W  -> 0b0100_0000_0000_0000_0000_0000_0000
  | STR, T  -> check_post_addr () ; 0b0100_0010_0000_0000_0000_0000_0000
  | STR, BT -> check_post_addr () ; 0b0100_0110_0000_0000_0000_0000_0000
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

let mov_mvn_to_binary instr s cond rd rs =
  let opcodes = match instr with
  | MOV -> [
      0b0000_0001_1010_0000_0000_0000_0000_0000 ;
      0b0000_0001_1010_0001_0000_0000_0000_0000 (* SBZ does not really need to be 0 *)
    ]
  | MVN -> [ 0b0001_1110_0000_0000_0000_0000_0000 ] in
  let scode = if s then 1 else 0 in
  let scode = shift_left (of_int scode) 20 in
  let vs = opcodes |>
    List.map (fun opcode ->
    of_int opcode |>
    add_condition_code cond |>
    add_rd_code rd |>
    logor scode) in
  let addr_modes = addr_mode_1 rs in
  vs |> List.map (fun v ->
    addr_modes |> List.map (fun addr_mode -> logor v addr_mode)
  ) |> List.flatten

let calculation_to_binary instr s cond rd rn op2 =
  let opcode = match instr with
  | ADC -> 0b0000_1010_0000_0000_0000_0000_0000
  | SBC -> 0b0000_1100_0000_0000_0000_0000_0000
  | BIC -> 0b0001_1100_0000_0000_0000_0000_0000
  | AND -> 0b0000_0000_0000_0000_0000_0000_0000
  | ADD -> 0b0000_1000_0000_0000_0000_0000_0000
  | SUB -> 0b0000_0100_0000_0000_0000_0000_0000
  | ORR -> 0b0001_1000_0000_0000_0000_0000_0000
  | EOR -> 0b0000_0010_0000_0000_0000_0000_0000
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

let branch_to_binary l cond target =
  let opcode =
    if l
    then 0b1011_0000_0000_0000_0000_0000_0000
    else 0b1010_0000_0000_0000_0000_0000_0000
  in
  let v = of_int opcode |>
    add_condition_code cond in
  let imm = signed_immed24 (sub target 8l) in
  [logor v imm]

let branchx_to_binary l cond rm =
  let opcode =
    if l
    then 0b0001_0010_1111_1111_1111_0011_0000
    else 0b0001_0010_1111_1111_1111_0001_0000
  in
  [of_int opcode |>
  add_condition_code cond |>
  add_rm_code rm]

let arm_to_binary arm =
  match arm with
  | Custom i -> [i]
  | Mem {instr;typ;cond;rd;ro} -> ldr_str_to_binary instr typ cond rd ro
  | Mov {instr;s;cond;rd;rs}   -> mov_mvn_to_binary instr s cond rd rs
  | DataProc {instr;s;cond;rd;rn;op2} -> calculation_to_binary instr s cond rd rn op2
  | Branch {l;cond;target} -> branch_to_binary l cond target
  | BranchX {l;cond;rm} -> branchx_to_binary l cond rm

let reverse_endianness v =
  let v1 = shift_left (logand mask8 v) (3*8) in
  let v = shift_right_logical v 8 in
  let v2 = shift_left (logand mask8 v) (2*8) in
  let v = shift_right_logical v 8 in
  let v3 = shift_left (logand mask8 v) (1*8) in
  let v = shift_right_logical v 8 in
  let v4 = logand mask8 v in
  logor v1 v2 |> logor v3 |> logor v4
