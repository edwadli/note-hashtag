%{ open Ast %}

%token SEP
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE DOT_LPAREN CONCAT
%token PLUS MINUS TIMES DIVIDE MOD
%token EQ NEQ LT LTE GT GTE
%token NOT AND OR
/* Note: "a = b = 3" is valid; 3 is assigned to b, and the value of that */
/* expression is assigned to a. */
%token ASSIGN
%token IF THEN ELSE BE UNLESS INWHICHCASE FOR IN DO
%token TYPE
%token BLING
%token TYPE
%token BLING
%token EOF
%token INCLUDE FUN

%token <bytes> ID_VAR
/*%token <Ast.assignable> ID_VAR_ASSIGNABLE*/
%token <bytes> ID_FUN

%token <bool> LIT_BOOL
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <bytes> LIT_STR

%nonassoc ELSE INWHICHCASE DO
%left SEP
%right ASSIGN
%left CONCAT
%left OR
%left AND
/* 1 == 1 == true is valid */
%left EQ NEQ
/* x < y < z can never be valid because can't use < on bool type. */
%nonassoc LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left BLING

/* Unary Operators */
%nonassoc NOT
%right prec_unary_minus

%start program
%type <Ast.program> program

%%

program:
| program_header program_body { (fun incls (fdefs, exprs, structdefs) -> (incls, fdefs, exprs, structdefs)) $1 $2 } 

/*program:
| program_header program_body { (fun incls (fdefs, exprs) -> (incls, fdefs, exprs)) $1 $2 } */

program_header:
| include_list { $1 }

program_body:
| EOF { [], [], [] }
| struct_construct sep_plus program_body { (fun (fdefs, exprs, structdefs) -> (fdefs, exprs, $1 :: structdefs)) $3 }
| fun_def sep_plus program_body { (fun (fdefs, exprs, structdefs) -> ($1 :: fdefs, exprs, structdefs)) $3 }
| expr    sep_plus program_body { (fun (fdefs, exprs, structdefs) -> (fdefs, $1 :: exprs, structdefs)) $3 }

/*program_body:
| EOF { [], []}
| fun_def sep_plus program_body { (fun (fdefs, exprs) -> ($1 :: fdefs, exprs)) $3 }
| expr    sep_plus program_body { (fun (fdefs, exprs) -> (fdefs, $1 :: exprs)) $3 }*/


struct_construct: 
| TYPE ID_VAR LBRACE ass_list RBRACE { New_struct($2, List.rev $4) }

/*struct_construct:
| TYPE ID_VAR LBRACE assignment RBRACE { New_struct($2, $4) }*/

fun_def:
| FUN ID_FUN id_var_list EQ expr { FunDef($2, $3, $5) }

include_list:
| /* nothing */ { [] }
| SEP include_list { $2 }
| INCLUDE ID_VAR sep_plus include_list { $2 :: $4 }

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
| expr CONCAT expr { Binop($1, Concat, $3) }
| ID_VAR DOT_LPAREN expr RPAREN { ArrIdx($1, $3) }
| LBRACK stmt_list RBRACK { Arr(List.rev $2) }
| LBRACE stmt_list RBRACE { ArrMusic(List.rev $2) }
| control { $1 }

control:
| IF sep_expr_sep THEN sep_expr_sep ELSE sep_star expr { Conditional($2,$4,$7) }
| BE sep_expr_sep UNLESS sep_expr_sep INWHICHCASE sep_star expr { Conditional($4,$7,$2) }
| FOR sep_star ID_VAR sep_star IN sep_expr_sep DO sep_star expr { For($3,$6,$9) }

id_var_list:
| /* nothing */ { [] }
| ID_VAR id_var_list { $1 :: $2 }

stmt_list:
| /* nothing */ { [] }
| stmt_list non_apply { $2 :: $1 }

apply:
| ID_FUN args_list { FunApply($1, $2) }

args_list:
| /* nothing */       { [] }
| non_apply args_list { $1 :: $2 }

non_apply:
| LPAREN block RPAREN { $2 } /* we get unit () notation for free (see block) */
| lit                { $1 }
| ID_VAR             { IdVar($1) }
| ID_VAR BLING ID_VAR { StructAccess($1, $3) }

sep_expr_sep:
| sep_star expr sep_star { $2 }

lit:
| LIT_BOOL         { LitBool($1) }
| LIT_INT          { LitInt($1) }
| LIT_FLOAT        { LitFloat($1) }
| LIT_STR          { LitStr($1) }

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

assignable:
| ID_VAR { IdVar_assignable($1) }
| ID_VAR BLING assignable { StructAccess_assignable($1, $3) }

assignment:
| assignable ASSIGN expr { Assign($1, $3) }

ass_list:
| assignment { [$1] }
| ass_list SEP assignment { $3 :: $1 } 


