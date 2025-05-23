
type unop = OId | ONeg | ONot | OBNot
type binop = OAdd | OSub | OMul | ODiv | OMod
           | OAnd | OXor | OOr | OLShift | ORShift
           | OEq | ONeq | OBOr | OBAnd
           | OGeq | OGt | OLeq | OLt

type meta_expr =
  | MConst of int32
  | MBinary of binop * meta_expr * meta_expr
  | MUnary of unop * meta_expr
  | MVar of string
  | MCond of meta_expr * meta_expr * meta_expr

type def_val = HNone | HString of string | HInt of int32 | HBool of bool
type definition = Param of string * def_val | VarDef of string * bool * meta_expr
type headers = definition list

module StrMap = Map.Make(String)
type env = int32 StrMap.t
exception VarNotFound of string

let get_param headers name =
  let rec aux lst =
    match lst with
    | [] -> HNone
    | (VarDef _)::lst -> aux lst
    | (Param (n,v))::_ when String.equal n name -> v
    | (Param _)::lst -> aux lst
  in
  aux headers

let eval_unary op i =
  match op with
  | OId -> i
  | ONeg -> Int32.neg i
  | ONot -> Int32.lognot i
  | OBNot -> if Int32.equal i Int32.zero then Int32.one else Int32.zero

let eval_binary op i1 i2 =
  match op with
  | OAdd -> Int32.add i1 i2
  | OSub -> Int32.sub i1 i2
  | OMul -> Int32.mul i1 i2
  | ODiv -> Int32.unsigned_div i1 i2
  | OMod -> Int32.unsigned_rem i1 i2
  | OAnd -> Int32.logand i1 i2
  | OXor -> Int32.logxor i1 i2
  | OOr -> Int32.logor i1 i2
  | OLShift -> Int32.shift_left i1 (Utils.uint32_to_int i2)
  | ORShift -> Int32.shift_right_logical i1 (Utils.uint32_to_int i2)
  | OEq -> if Int32.equal i1 i2 then Int32.one else Int32.zero
  | ONeq -> if Int32.equal i1 i2 then Int32.zero else Int32.one
  | OBOr ->
    if Int32.equal i1 Int32.zero && Int32.equal i2 Int32.zero
    then Int32.zero else Int32.one
  | OBAnd ->
    if Int32.equal i1 Int32.zero || Int32.equal i2 Int32.zero
    then Int32.zero else Int32.one
  | OGeq -> if Int32.unsigned_compare i1 i2 >= 0 then Int32.one else Int32.zero
  | OGt -> if Int32.unsigned_compare i1 i2 > 0 then Int32.one else Int32.zero
  | OLeq -> if Int32.unsigned_compare i1 i2 <= 0 then Int32.one else Int32.zero
  | OLt -> if Int32.unsigned_compare i1 i2 < 0 then Int32.one else Int32.zero

let eval_meta_expr env e =
  let rec aux e =
    match e with
    | MConst i -> i
    | MVar str ->
      if StrMap.mem str env
      then StrMap.find str env
      else raise (VarNotFound str)
    | MUnary (op, e) ->
      let i = aux e in
      eval_unary op i
    | MBinary (op, e1, e2) ->
      let i1 = aux e1 and i2 = aux e2 in
      eval_binary op i1 i2
    | MCond (e0, e1, e2) ->
      let i0 = aux e0 in
      if Int32.equal i0 Int32.zero then aux e2 else aux e1
  in
  aux e

let empty_env = StrMap.empty

let env_from_headers fmt headers =
  let treat_def (printed, acc) def =
    match def with
    | Param _ -> (printed, acc)
    | VarDef (str, print, expr) ->
      let i = eval_meta_expr acc expr in
      if print then Format.fprintf fmt "%s = %li (%#lx)@." str i i ;
      (printed || print, StrMap.add str i acc)
  in
  let (printed, res) =
    List.fold_left treat_def (false, StrMap.empty) headers in
  if printed then Format.fprintf fmt "@." ; res

let concat_env env1 env2 =
  StrMap.fold (fun k v acc -> StrMap.add k v acc) env2 env1
