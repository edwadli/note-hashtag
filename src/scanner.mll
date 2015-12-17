{
  open Core.Std
  
  open Parser
  
  exception Lexing_error of string
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase
let digit = ['0'-'9']
let newline = ('\n' | '\r' | "\r\n")
let whitespace = [' ' '\t']
let separator = (newline | ';')

(* Used for float parsing *)
let hasint = digit+ '.' digit*
let hasfrac = digit* '.' digit+
let hasexp = 'e' ('+'? | '-') digit+

rule token = parse
| '\\' newline { token lexbuf }
| separator { SEP }
| whitespace { token lexbuf }
| eof { EOF }
(* Scoping *)
| '(' { LPAREN }
| ')' { RPAREN }
| '{' (whitespace | newline)* '}'{ BRACES }
| '[' { LBRACK }
| ']' { RBRACK }
| '{' { LBRACE }
| '}' { RBRACE }
(* Operators *)
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }
| '=' { ASSIGN }
| "==" { EQ }
| "!=" { NEQ }
| '<' { LT }
| "<=" { LTE }
| '>' { GT }
| ">=" { GTE }
| ".(" { DOT_LPAREN }
| '.' { CONCAT }
| ',' {COMMA}
| '!' { NOT }
| "&&" { AND }
| "||" { OR }
| '#' {SHARP}
| 'b' {FLAT}
| ':' {COLON}
| '@' {OCTAVE}
| '~' { TILDE }
| "->" { RARROW }
| "<-" { LARROW }
| '$' { BLING }
(* Keywords *)
(* Regex conflicts are resolved by order! Place all keywords in this section or ID_LOWER will eat them up. *)
| "unit" { TYPE_UNIT }
| "bool" { TYPE_BOOL }
| "int" { TYPE_INT }
| "float" { TYPE_FLOAT }
| "string" { TYPE_STR }
| "true" { LIT_BOOL(true) }
| "false" { LIT_BOOL(false) }
| "fun" { FUN }
| "include" { INCLUDE }
| "if" { IF }
| "then" { THEN }
| "else"{ ELSE }
| "be" { BE }
| "unless" { UNLESS }
| "inwhichcase" { INWHICHCASE }
| "for" { FOR }
| "in" { IN }
| "do" { DO }
| "throw" { THROW }
| "type" { TYPE }
| "init" | "beget" | "bringintobeing" { INIT }
| "extern" { EXTERN }
| "const" { CONST }
(* Literals *)
| digit+ as lit { LIT_INT(Int.of_string lit) }
| ((hasint | hasfrac) hasexp?) | (digit+ hasexp) as lit { LIT_FLOAT(Float.of_string lit) }
| '"' (('\\' '"'| [^'"'])* as str) '"' { LIT_STR(Scanf.unescaped str) }
(* Identifiers *)
| (lowercase | '_') (letter | digit | '_')* as lit { ID_LOWER(lit) }
| uppercase (letter | digit | '_')* as lit { ID_UPPER(lit) }
(* Comments *)
| "//" { comment_oneline lexbuf }
| "/*" { comment_multiline 0 lexbuf }
| "*/" { raise (Lexing_error("You have a '*/' without a matching '/*'")) }
| _ as c { raise (Lexing_error("Unknown token '" ^ Char.to_string c ^ "'")) }

and comment_oneline = parse
| (newline | eof) { SEP } (* End of single line comment *)
| _ { comment_oneline lexbuf }

and comment_multiline depth = parse
| "/*" { comment_multiline (depth + 1) lexbuf }
| "*/" { if depth = 0 then token lexbuf else comment_multiline (depth - 1) lexbuf }
| eof { raise (Lexing_error("You have a '/*' without a matching '*/'")) }
| _ { comment_multiline depth lexbuf }
