
type unop = OId | ONeg | ONot
type binop = OAdd | OSub | OMul | ODiv | OMod
           | OAnd | OXor | OOr | OLShift | ORShift

type meta_expr =
  | MConst of int32
  | MBinary of binop * meta_expr * meta_expr
  | MUnary of unop * meta_expr
  | MVar of string

type def_val = HNone | HString of string | HInt of int32 | HBool of bool
type definition = Param of string * def_val | VarDef of string * meta_expr
type headers = definition list

let get_param headers name =
  let rec aux lst =
    match lst with
    | [] -> HNone
    | (VarDef _)::lst -> aux lst
    | (Param (n,v))::_ when String.equal n name -> v
    | (Param _)::lst -> aux lst
  in
  aux headers
