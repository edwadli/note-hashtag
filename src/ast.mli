type operator = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Lte

type expr =
  | Binop of expr * operator * expr
  | LitBool of bool
  | LitInt of int
  | LitFloat of float
  | LitStr of bytes
  | Asn of int * expr
  | IdVar of bytes
  | IdFun of bytes
  | FunApply of bytes * expr list
  | ArrIdx of bytes * expr
  | Arr of expr list
  | ArrMusic of expr list
