type binary_operator =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte
  | And | Or
  | Concat

type unary_operator =
  | Not
  | Neg

type expr =
  | Binop of expr * binary_operator * expr
  | Uniop of unary_operator * expr
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
  | Block of expr list
  | Conditional of expr * expr * expr
  | For of bytes * expr * expr

type fundef =
  | FunDef of bytes * bytes list * expr

type program = bytes list * fundef list * expr list

(* struct is actually a keywork in ocaml, called it struct_type instead *)
type struct_type =
  | New_struct of assign list
