{ open Parser }

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
| "true" { LIT_BOOL(true) }
| "false" { LIT_BOOL(false) }
| "fun" { FUN }
| "include" { INCLUDE }
| digit+ as lit { LIT_INT(int_of_string lit) }
| ((hasint | hasfrac) hasexp?) | (digit+ hasexp) as lit { LIT_FLOAT(float_of_string lit) }
(* matches only outer quotes *)
| '"' (('\\' '"'| [^'"'])* as str) '"' { LIT_STR(Scanf.unescaped(str)) }
| (lowercase | '_') (letter | digit | '_')* as lit { ID_VAR(lit) }
| uppercase (letter | digit | '_')* as lit { ID_FUN(lit) }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACK }
| ']' { RBRACK }
| '{' { LBRACE }
| '}' { RBRACE }
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
| "//" { comment_oneline lexbuf }
| "/*" { comment_multiline 0 lexbuf }
| eof { EOF }

and comment_oneline = parse
| (newline | eof) { token lexbuf } (* End of single line comment *)
| _ eof { comment_oneline lexbuf }

and comment_multiline depth = parse
| "/*" { comment_multiline (depth + 1) lexbuf }
| "*/" { if depth = 0 then token lexbuf else comment_multiline (depth - 1) lexbuf }
| eof { print_endline "You have a /* without a matching */"; raise End_of_file }
| _ { comment_multiline depth lexbuf }
