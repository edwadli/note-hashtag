
open Ast

type variable_name = Ast.var_reference
type new_variable_name = string
type function_name =
  | NhFunction of string
  (* Header file name, namespace, C++ function name *)
  | CppFunction of string * string * string

type expr_detail =
  | LitBool of bool
  | LitInt of int
  | LitFloat of float
  | LitStr of string
  | LitUnit
  | Binop of expr_typed * Ast.binary_operator * expr_typed
  | Uniop of Ast.unary_operator * expr_typed
  | VarRef of variable_name
  | FunApply of function_name * expr_typed list
  | ArrIdx of variable_name * expr_typed
  | Arr of (expr_typed list) * Ast.t
  | Block of expr_typed list
  | Conditional of expr_typed * expr_typed * expr_typed
  | For of new_variable_name * expr_typed * expr_typed
  | Throw of expr_typed
  | Init of new_variable_name * expr_typed
  | Assign of variable_name * expr_typed
  | Struct of type_name * ((string * expr_typed) list)

and expr_typed = expr_detail * Ast.t

(* type name, fields and default values *)
type tdefault = TDefault of type_name * ((string * expr_typed) list)

type fundef_typed = FunDef of string * ((string * Ast.t) list) * expr_typed

(* C++ includes, function declarations, expressions, types (with defaults) *)
type program_typed = string list * fundef_typed list * expr_typed list * tdefault list
