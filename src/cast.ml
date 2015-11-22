open Core.Std

open Ast

type binary_operator = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
type unary_operator = Not | Neg

type decl = Ast.t * string


type expr =
  | LitUnit
  | LitBool of bool
  | LitInt of int
  | LitFloat of float
  | LitStr of string
  | InitList of expr list
  | Decl of decl
  | DeclAssign of decl * expr
  | VarRef of var_reference
  | Idx of var_reference * expr
  | Binop of expr * binary_operator * expr
  | Uniop of unary_operator * expr
  | Assign of var_reference * expr
  | Call of callable * expr list
  | Noexpr

and stmt =
  | Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | ForRange of decl * expr * stmt
  | While of expr * stmt

and callable = 
  | Struct of string
  | Function of string * string
  | Method of var_reference * string
  | LambdaRefCap of decl list * Ast.t * stmt list

type func_decl = {
  fnamespace : string;
  fname : string;
  fargs : decl list;
  treturn : Ast.t;
  body : stmt list;
}

type struct_decl = {
  sname: string;
  sargs: decl list;
}

type incl =
  | IncludeAngleBrack of string
  | IncludeQuote of string

type signature =
  | SigStruct of string
  | SigFunc of string * Ast.t * decl list

type program = incl list * signature list * decl list * struct_decl list * func_decl list

let sep = ";\n"
let ns = "::"

let rec string_of_type t =
  match t with
  | Unit -> "unit"
  | Int -> "int64_t"
  | Float -> "double"
  | String -> "std" ^ ns ^ "string"
  | Bool -> "bool"
  | Type(type_name) -> type_name
  | Array(t) -> "std" ^ ns ^ "vector<" ^ string_of_type t ^ ">"

let rec string_of_expr = function
  | LitUnit -> "LIT_UNIT"
  | LitBool(x) -> Bool.to_string x
  | LitInt(x) -> Int.to_string x
  | LitFloat(x) -> sprintf "%.17F" x
  | LitStr(x) -> "\"" ^ String.escaped x ^ "\""
  | InitList(exprs) -> "{ " ^ String.concat ~sep:", " (List.map exprs ~f:string_of_expr) ^ " }"
  | Decl(t, name) -> string_of_type t ^ " " ^ name
  | DeclAssign((t, name), e) -> string_of_type t ^ " " ^ name ^ " = " ^ string_of_expr e
  | VarRef(names) -> String.concat ~sep:"." names
  | Idx(name, e) -> string_of_expr (VarRef(name)) ^ "[" ^ string_of_expr e ^ "]"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
      | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Uniop(o, e) -> (match o with Not -> "!" | Neg -> "-") ^ string_of_expr e
  | Assign(v, e) -> string_of_expr (VarRef(v)) ^ " = " ^ string_of_expr e
  | Call(callexpr, args) -> string_of_callable callexpr ^ "(" ^
      String.concat ~sep:", " (List.map args ~f:string_of_expr) ^ ")"
  | Noexpr -> ""

and string_of_callable = function
  | Struct(name) -> name
  | LambdaRefCap(decls, treturn, stmts) -> "[&] (" ^
      String.concat ~sep:", " (List.map decls ~f:(fun (t, name) -> string_of_type t ^ " " ^ name)) ^
      ") -> " ^ string_of_type treturn ^" "^ string_of_stmt (Block stmts)
  | Method(oname, fname) -> string_of_expr (VarRef(oname)) ^ "." ^ fname
  | Function(namespace, fname) -> namespace ^ ns ^ fname

and string_of_stmt = function
  | Block(stmts) -> "{\n" ^ String.concat (List.map stmts ~f:string_of_stmt) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ sep
  | Return(expr) -> "return " ^ string_of_expr expr ^ sep
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") " ^ string_of_stmt s
  | ForRange(rdecl, rexpr, s) ->
      "for (" ^ string_of_expr (Decl(rdecl)) ^ " : " ^ string_of_expr rexpr ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_fdecl fdecl =
  string_of_type fdecl.treturn ^ " " ^ fdecl.fnamespace ^ ns ^ fdecl.fname ^ "(" ^
  String.concat ~sep:", " (List.map fdecl.fargs ~f:(fun (t, name) -> string_of_type t ^ " " ^ name)) ^ ")\n{\n" ^
    String.concat (List.map fdecl.body ~f:string_of_stmt) ^
  "}\n"

let string_of_sdecl sdecl = 
  "struct " ^ sdecl.sname ^ " {\n" ^
    String.concat ~sep:"\n" (List.map sdecl.sargs ~f:(fun decl -> string_of_stmt (Expr(Decl(decl))))) ^
    "\n" ^ sdecl.sname ^ "(" ^
    String.concat ~sep:", " (List.map sdecl.sargs ~f:(fun decl -> string_of_expr (Decl(decl)))) ^
    ") : " ^ String.concat ~sep:", " (List.map sdecl.sargs ~f:(fun (_,n) -> n^"("^n^")")) ^
    " {}" ^ "\n};\n"

let string_of_signature = function
  | SigStruct(name) -> "struct "^name
  | SigFunc(name, tret, decls) ->
      string_of_type tret ^ " " ^ name ^ "(" ^
      String.concat ~sep:", " (List.map decls ~f:(fun (t,_) -> string_of_type t)) ^
      ")"

let string_of_incl incl =
  match incl with
  | IncludeAngleBrack(path) -> "#include <" ^ path ^ ">"
  | IncludeQuote(path) -> "#include \"" ^ path ^ "\""

let string_of_program (incls, signatures, decls, structs, funcs) =
  String.concat ~sep:"\n" (List.map incls ~f:string_of_incl) ^
  String.concat ~sep:";\n" (List.map signatures ~f:string_of_signature) ^
  String.concat ~sep:"\n" (List.map decls ~f:(fun decl -> string_of_stmt (Expr(Decl(decl))))) ^ "\n" ^
  String.concat ~sep:"\n" (List.map structs ~f:string_of_sdecl) ^
  String.concat ~sep:"\n" (List.map funcs ~f:string_of_fdecl)
