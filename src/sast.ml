open Ast

type variable_name = VarName of Ast.var_reference
type new_variable_name = NewVarName of string
type function_name = string

type expr_detail =
  | LitBool of bool
  | LitInt of int
  | LitFloat of float
  | LitStr of string
  | Binop of expr_typed * Ast.binary_operator * expr_typed
  | Uniop of Ast.unary_operator * expr_typed
  | VarRef of variable_name
  | FunApply of function_name * expr_typed list
  | ArrIdx of variable_name * expr_typed
  | Arr of (expr_typed list)
  | ArrMusic of (expr_typed list)
  | EmptyList of t
  | EmptyMusicList of t
  | Block of expr_typed list
  | Conditional of expr_typed * expr_typed * expr_typed
  | For of new_variable_name * expr_typed * expr_typed
  | Throw of expr_typed * expr_typed
  | InitAssign of new_variable_name * expr_typed
  | Assign of variable_name * expr_typed
  | StructInit of type_name * expr_typed list

and expr_typed = expr_detail * t

(* type name, fields and default values *)
type tdefault = TDefault of type_name * ((string * expr_typed) list)

type fundef_typed = FunDef of function_name * ((string * t) list) * expr_typed

(* function declarations, expressions, types, type defaults *)
type program_typed = fundef_typed list * expr_typed list * tdefault list
