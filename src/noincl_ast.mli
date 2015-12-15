open Core.Std

type program = Ast.fundef list * Ast.externfun list * Ast.expr list * Ast.typedef list

val ast_of_filename : string option -> program
