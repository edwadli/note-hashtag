type binary_operator =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte
  | And | Or
  | Concat

type unary_operator =
  | Not
  | Neg

type var_reference = 
  | IdVar of bytes
  | StructAccess of bytes * var_reference

type expr =
  | Binop of expr * binary_operator * expr
  | Uniop of unary_operator * expr
  | LitBool of bool
  | LitInt of int
  | LitFloat of float
  | LitStr of bytes
  | VarRef of var_reference
  | IdFun of bytes
  | FunApply of bytes * expr list
  | ArrIdx of bytes * expr
  | Arr of expr list
  | ArrMusic of expr list
  | Block of expr list
  | Conditional of expr * expr * expr
  | For of bytes * expr * expr
  | Assign of var_reference * expr
  | StructInit of bytes * expr list

type fundef =
  | FunDef of bytes * bytes list * expr

(* struct is actually a keyword in ocaml, called it struct_type instead *)
type struct_type =
  | New_struct of bytes * expr list

type program = bytes list * fundef list * expr list * struct_type list


