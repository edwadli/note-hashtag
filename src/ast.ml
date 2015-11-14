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

type var_reference = string list

type expr =
  | Binop of expr * binary_operator * expr
  | Uniop of unary_operator * expr
  | LitBool of bool
  | LitInt of int
  | LitFloat of float
  | LitStr of string
  | VarRef of var_reference
  | IdFun of string
  | FunApply of string * expr list
  | ArrIdx of string * expr
  | Arr of expr list
  | ArrMusic of expr list
  | Block of expr list
  | Conditional of expr * expr * expr
  | For of string * expr * expr
  | Throw of expr * expr
  | Assign of var_reference * expr
  | StructInit of string * expr list

type fundef =
  | FunDef of string * string list * expr

type typedef =
  | TypeDef of string * expr list

type program = string list * fundef list * expr list * typedef list

let a = Array.make 26 0

let string_of_op o =
  match o with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq  -> "="
  | Neq -> "!="
  | Lt  -> "<"
  | Lte -> "<="
  | And -> "&&"
  | Or  -> "||"
  | Concat -> "."
  | Chord -> ","
  | Zip -> ":"
  | Octave -> "@"

let string_of_unop o = match o with | Not -> "!" | Neg -> "-" | Sharp -> "#"
				    | Flat -> "b"

let rec string_of_expr e =
  match e with
  | Block l -> String.concat " " [ "["; string_of_exp_list l; "]" ]
  | Conditional(x, y, z) -> String.concat " " [ "IF"; string_of_expr x; "THEN"; string_of_expr y; "ELSE"; string_of_expr z ]
  | For(x, y, z) -> String.concat " " [ "FOR"; x; "IN "; string_of_expr y; "DO"; string_of_expr z ]
  | Binop(x, op, y) -> String.concat " " [ "("; string_of_expr x; string_of_op op; string_of_expr y; ")" ]
  | Uniop(op, x) -> String.concat " " [ "("; string_of_unop op; string_of_expr x; ")" ]
  | LitBool(x) -> string_of_bool x
  | LitInt(x) -> string_of_int x
  | LitFloat(x) -> string_of_float x
  | LitStr(x) -> x
  | VarRef(x) -> String.concat "$" x
  | Assign(x, y) -> String.concat " " [ "("; string_of_expr (VarRef(x)); "="; string_of_expr y; ")" ]
  | StructInit(x, y) -> String.concat " " [ x; string_of_exp_list y ]
  | IdFun(x) -> x
  | FunApply(x, y) -> String.concat " " [ x; "("; string_of_exp_list y; ")" ]
  | ArrIdx (x, y) -> String.concat " " [ x; ".("; string_of_expr y; ")" ]
  | Arr(x) -> String.concat " " [ "["; string_of_exp_list x; "]" ]
  | ArrMusic(x) -> String.concat " " [ "{"; string_of_exp_list x; "}" ]
  | Throw(x, y) -> String.concat " " ["Throw"; string_of_expr x; string_of_expr y]
and string_of_exp_list l =
  match l with
  | [] -> ""
  | [ s ] -> string_of_expr s
  | s :: rest -> String.concat " " [ string_of_expr s; ","; string_of_exp_list rest ]

let rec string_of_fdef f =
  match f with
  | [] -> ""
  | FunDef(x, y, z) :: rest ->
      String.concat " " [ x; string_of_var_list y; string_of_expr z; string_of_fdef rest ]
and string_of_var_list v =
  match v with
  | [] -> ""
  | s :: rest -> String.concat " " [ s; string_of_var_list rest ]

let rec string_of_incl_list p =
  match p with
  | [] -> "[]"
  | s :: rest -> String.concat " " [ s; string_of_incl_list rest ]

let rec string_of_typedefs typedefs =
  match typedefs with
  | [] -> ""
  | TypeDef(name, exprs) :: rest -> name ^ string_of_exp_list exprs ^ "\n" ^ string_of_typedefs rest

let string_of_prog_struc p =
  match p with
  | (incls, fdefs, exprs, typedefs) ->
      String.concat "\n" [ "INCLUDES:" ^ string_of_incl_list incls;
                           "TYPEDEFS: " ^ string_of_typedefs typedefs;
                           "FDEF:" ^ string_of_fdef fdefs;
                           "EXPR: " ^ string_of_exp_list exprs;
                         ]
