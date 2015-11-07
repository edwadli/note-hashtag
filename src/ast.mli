type binary_operator =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte
  | And | Or | Zip
  | Concat | Chord | Octave

type unary_operator =
  | Not
  | Neg
  | Sharp
  | Flat

type var_reference = bytes list

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
  | Throw of expr * expr
  | Assign of var_reference * expr
  | StructInit of bytes * expr list

type fundef =
  | FunDef of bytes * bytes list * expr

type typedef =
  | TypeDef of bytes * expr list

type program = bytes list * fundef list * expr list * typedef list
