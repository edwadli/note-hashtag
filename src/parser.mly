%{
  open Core.Std
  
  open Ast
%}

%token SEP
%token LPAREN RPAREN BRACKS BRACES LBRACK RBRACK LBRACE RBRACE DOT_LPAREN CONCAT COMMA
%token PLUS MINUS TIMES DIVIDE MOD
%token EQ NEQ LT LTE GT GTE
%token NOT AND OR
%token SHARP FLAT COLON OCTAVE
/* Note: "a = b = 3" is valid; 3 is assigned to b, and the value of that */
/* expression is assigned to a. */
%token ASSIGN CONST
%token TILDE
%token IF THEN ELSE BE UNLESS INWHICHCASE FOR IN DO
%token TYPE
%token BLING
%token EOF
%token INCLUDE FUN EXTERN LARROW RARROW
%token THROW
%token INIT

%token <string> ID_LOWER
%token <string> ID_UPPER

%token <bool> LIT_BOOL
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <string> LIT_STR
%token TYPE_UNIT TYPE_BOOL TYPE_INT TYPE_FLOAT TYPE_STR

%nonassoc ELSE INWHICHCASE DO
%left SEP
%nonassoc ASSIGN
%left OR
%left AND
%left EQ NEQ
/* x < y < z can never be valid because can't use < on bool type. */
%nonassoc LT LTE GT GTE
%left CONCAT
%left COLON
%left COMMA
%left OCTAVE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left BLING

/* Unary Operators */
%nonassoc NOT
%right prec_unary_minus
%left SHARP FLAT

%start program
%type <Ast.program> program

%%

program:
| sep_star EOF { [], [], [], [], [] }
| sep_star program_header_follow_body program_body EOF
  { (fun (fdefs, externs, exprs, structdefs) -> ($2, fdefs, externs, exprs, structdefs)) $3 }
| sep_star program_body EOF { (fun (fdefs, externs, exprs, structdefs) -> ([], fdefs, externs, exprs, structdefs)) $2 }
| sep_star program_header EOF { $2, [], [] ,[], [] }

program_header_follow_body:
| INCLUDE ID_LOWER include_list sep_plus { $2 :: List.rev $3 }

program_header:
| INCLUDE ID_LOWER include_list sep_star { $2 :: List.rev $3 }

include_list:
| /* nothing */ { [] }
| include_list sep_plus INCLUDE ID_LOWER { $4 :: $1 }

program_body:
| struct_declaration program_body_list sep_star
  { (fun (fdefs, externs, exprs, structdefs) -> (fdefs, externs, exprs, $1 :: structdefs)) $2 }
| fun_def program_body_list sep_star
  { (fun (fdefs, externs, exprs, structdefs) -> ($1 :: fdefs, externs, exprs, structdefs)) $2 }
| extern_fun program_body_list sep_star
  { (fun (fdefs, externs, exprs, structdefs) -> (fdefs, $1 :: externs, exprs, structdefs)) $2 }
| expr program_body_list sep_star
  { (fun (fdefs, externs, exprs, structdefs) -> (fdefs, externs, $1 :: exprs, structdefs)) $2 }

program_body_list:
| /* nothing */ { [], [], [], [] }
| program_body_list sep_plus struct_declaration
  { (fun (fdefs, externs, exprs, structdefs) -> (fdefs, externs, exprs, structdefs @ [ $3 ])) $1 }
| program_body_list sep_plus fun_def
  { (fun (fdefs, externs, exprs, structdefs) -> (fdefs @ [ $3 ], externs, exprs, structdefs)) $1 }
| program_body_list sep_plus extern_fun
  { (fun (fdefs, externs, exprs, structdefs) -> (fdefs, externs @ [ $3 ], exprs, structdefs)) $1 }
| program_body_list sep_plus expr
  { (fun (fdefs, externs, exprs, structdefs) -> (fdefs, externs, exprs @ [ $3 ], structdefs)) $1 }

struct_declaration:
| TYPE ID_LOWER ASSIGN LBRACE sep_star asn_list sep_star RBRACE { TypeDef($2, List.rev $6) }

fun_def:
| FUN ID_UPPER ID_LOWER_list ASSIGN expr { FunDef($2, $3, $5) }

extern_fun:
| EXTERN LIT_STR LIT_STR LIT_STR FUN ID_UPPER typename_list RARROW typename { ExternFunDecl($2, $3, $4, $6, $7, $9) }

typename_list:
| /* nothing */ { [] }
| typename typename_list { $1 :: $2 }

typename:
| TYPE_UNIT  { Unit }
| TYPE_BOOL  { Bool }
| TYPE_INT   { Int }
| TYPE_FLOAT { Float }
| TYPE_STR   { String }
| ID_LOWER   { Type($1) }
| typename BRACES { Array($1) }

block:
| sep_list sep_star { Block(List.rev $1) } 
| expr sep_list sep_star { Block($1 :: List.rev $2) }

sep_list:
| /* nothing */ { [] }
| sep_list sep_plus expr { $3 :: $1 }

/* Helper: One or more separators */
sep_plus:
| SEP { () }
| SEP sep_plus { () }

/* Helper: Zero or more separators */
sep_star:
| /* nothing */ { () }
| SEP sep_star { () }

expr:
| apply     { $1 }
| non_apply { $1 }
| arith     { $1 }
| bool      { $1 }
| OCTAVE non_apply non_apply {Binop($3, Octave, $2)}
| expr COLON expr {Binop($1, Zip, $3)}
| asn_toplevel { $1 }
| expr CONCAT expr { Binop($1, Concat, $3) }
| control { $1 }
| THROW non_apply { Throw($2) }

control:
| IF sep_expr_sep THEN sep_expr_sep ELSE sep_star expr { Conditional($2,$4,$7) }
| BE sep_expr_sep UNLESS sep_expr_sep INWHICHCASE sep_star expr { Conditional($4,$7,$2) }
| FOR sep_star ID_LOWER sep_star IN sep_expr_sep DO sep_star expr { For($3,$6,$9) }
| INIT ID_LOWER { StructInit($2, []) }
| INIT ID_LOWER LBRACE sep_star asn_list sep_star RBRACE { StructInit($2, List.rev $5) }

ID_LOWER_list:
| /* nothing */ { [] }
| ID_LOWER ID_LOWER_list { $1 :: $2 }

stmt_list_plus:
| non_apply { [$1] }
| stmt_list_plus non_apply { $2 :: $1 }

apply:
| ID_UPPER args_list { FunApply($1, $2) }

args_list:
| /* nothing */       { [] }
| non_apply args_list { $1 :: $2 }

non_apply:
| var_ref { VarRef($1) }
| var_ref DOT_LPAREN expr RPAREN { ArrIdx($1, $3) }
| LPAREN block RPAREN { $2 } /* we get unit () notation for free (see block) */
| lit { $1 }
| non_apply OCTAVE non_apply { Binop($1, Octave, $3) }
| non_apply COMMA non_apply {Binop($1, Chord, $3)}
| music     { $1 }

sep_expr_sep:
| sep_star expr sep_star { $2 }

lit:
| LIT_BOOL         { LitBool($1) }
| LIT_INT          { LitInt($1) }
| LIT_FLOAT        { LitFloat($1) }
| LIT_STR          { LitStr($1) }
| TILDE            { StructInit("chord", []) }
| lit_array        { $1 }

lit_array:
| LBRACE stmt_list_plus RBRACE { Arr((List.rev $2), None) }
| LBRACK stmt_list_plus RBRACK { ArrMusic((List.rev $2)) }
| typename   BRACES  { Arr([], Some($1)) }

arith:
| MINUS expr %prec prec_unary_minus { Uniop(Neg, $2) }
| expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr MOD    expr { Binop($1, Mod, $3) }

bool:
| cmp { $1 }
| logic { $1 }

cmp:
| expr EQ  expr { Binop($1, Eq,  $3) }
| expr NEQ expr { Binop($1, Neq, $3) }
| expr LT  expr { Binop($1, Lt,  $3) }
| expr LTE expr { Binop($1, Lte, $3) }
| expr GT  expr { Binop($3, Lt,  $1) }
| expr GTE expr { Binop($3, Lte, $1) }

logic:
|      NOT expr { Uniop(Not, $2) }
| expr AND expr { Binop($1, And, $3) }
| expr OR  expr { Binop($1, Or,  $3) }

music:
| non_apply FLAT     { Uniop(Flat, $1)}
| non_apply SHARP    { Uniop(Sharp, $1)}

var_ref:
| ID_LOWER { [ $1 ] }
| ID_LOWER BLING var_ref { $1 :: $3 }

asn_toplevel:
| asn { $1 }
/* ID_LOWER because you can't do `const a$b = 10` */
| CONST ID_LOWER ASSIGN expr { Assign([ $2 ], $4, Immutable) }

asn:
| var_ref ASSIGN expr { Assign($1, $3, Mutable) }

asn_list:
| asn { [ $1 ] }
| asn_list sep_plus asn { $3 :: $1 }
