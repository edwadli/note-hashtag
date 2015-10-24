open Ast

let a = Array.make 26 0

let string_of_op o = match o with 
	Add -> "+"
	|Sub -> "-"
	|Mul -> "*"
	|Div -> "/"
	|Mod -> "%"
	|Eq -> "="
	|Neq -> "!="
	|Lt -> "<"
	|Lte -> "<="
	|And -> "&&"
	|Or -> "||"
	|Concat -> "."

let string_of_unop o = match o with
	Not -> "!"

let rec string_of_expr e s = match e with
	Block(l) -> String.concat " " ["["; string_of_exp_list l; "]"]
	|Binop(x, op, y) -> String.concat " " ["("; string_of_expr x "";string_of_op op;string_of_expr y ""; ")"]
	|Uniop(op, x) -> String.concat " " ["("; string_of_unop op; string_of_expr x ""; ")"]
	|LitBool(x) -> string_of_bool x
	|LitInt(x) -> string_of_int x
	|LitFloat(x) -> string_of_float x
	|LitStr(x) -> x
	|Asn(x, y) -> String.concat " " ["("; string_of_int x; "="; string_of_expr y ""; ")"]
	|IdVar(x) -> Bytes.to_string x
	|IdFun(x) -> Bytes.to_string x
	|FunApply(x,y) -> String.concat " " [Bytes.to_string x; "("; string_of_exp_list y; ")"]
	|ArrIdx(x, y) -> String.concat " " [Bytes.to_string x; ".("; string_of_expr y ""; ")"]
	|Arr(x) -> String.concat " " ["["; string_of_exp_list x; "]"]
	|ArrMusic(x) -> String.concat " " ["{"; string_of_exp_list x; "}"]
and string_of_exp_list l = match l with
	[] -> ""
	|[s] -> string_of_expr s ""
	|s::rest -> String.concat " " [string_of_expr s ""; ","; string_of_exp_list rest]


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.block Scanner.token lexbuf
	in let s = string_of_expr expr ""
	in print_endline s
(* string_of_expr expr "" *)
