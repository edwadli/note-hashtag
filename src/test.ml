open Ast

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
  | For(x, y, z) -> String.concat " " [ "FOR"; Bytes.to_string x; "IN "; string_of_expr y; "DO"; string_of_expr z ]
  | Binop(x, op, y) -> String.concat " " [ "("; string_of_expr x; string_of_op op; string_of_expr y; ")" ]
  | Uniop(op, x) -> String.concat " " [ "("; string_of_unop op; string_of_expr x; ")" ]
  | LitBool(x) -> string_of_bool x
  | LitInt(x) -> string_of_int x
  | LitFloat(x) -> string_of_float x
  | LitStr(x) -> x
  | Asn(x, y) -> String.concat " " [ "("; string_of_int x; "="; string_of_expr y; ")" ]
  | IdVar(x) -> Bytes.to_string x
  | IdFun(x) -> Bytes.to_string x
  | FunApply(x, y) -> String.concat " " [ Bytes.to_string x; "("; string_of_exp_list y; ")" ]
  | ArrIdx (x, y) -> String.concat " " [ Bytes.to_string x; ".("; string_of_expr y; ")" ]
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
      String.concat " " [ Bytes.to_string x; string_of_var_list y; string_of_expr z; string_of_fdef rest ]
and string_of_var_list v =
  match v with
  | [] -> ""
  | s :: rest -> String.concat " " [ Bytes.to_string s; string_of_var_list rest ]

let rec string_of_incl_list p =
  match p with
  | [] -> "[]"
  | s :: rest -> String.concat " " [ Bytes.to_string s; string_of_incl_list rest ]

let string_of_prog_struc p =
  match p with
  | (a, b, c) ->
      String.concat " " [ "INCLUDES:"; string_of_incl_list a; "\nFDEF:"; string_of_fdef b; "\nEXPR: "; string_of_exp_list c ]
