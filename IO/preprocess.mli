
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

type env
exception VarNotFound of string

val get_param : headers -> string -> def_val

val eval_meta_expr : env -> meta_expr -> int32

val empty_env : env
val env_from_headers : headers -> env
