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

%token <string> ID_VAR
%token <string> ID_FUN

%token <bool> LIT_BOOL
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <string> LIT_STR
%token TYPE_UNIT TYPE_BOOL TYPE_INT TYPE_FLOAT TYPE_STR

%nonassoc ELSE INWHICHCASE DO
%left SEP
%nonassoc ASSIGN
%left CONCAT
%left OR
%left AND
%left EQ NEQ
/* x < y < z can never be valid because can't use < on bool type. */
%nonassoc LT LTE GT GTE
%left OCTAVE
%left COLON
%left COMMA
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
| INCLUDE ID_VAR include_list sep_plus { $2 :: List.rev $3 }

program_header:
| INCLUDE ID_VAR include_list sep_star { $2 :: List.rev $3 }

include_list:
| /* nothing */ { [] }
| include_list sep_plus INCLUDE ID_VAR { $4 :: $1 }

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
| TYPE ID_VAR ASSIGN LBRACE sep_star asn_list sep_star RBRACE { TypeDef($2, List.rev $6) }

fun_def:
| FUN ID_FUN id_var_list ASSIGN expr { FunDef($2, $3, $5) }

extern_fun:
| EXTERN LIT_STR LIT_STR LIT_STR FUN ID_FUN typename_list RARROW typename { ExternFunDecl($2, $3, $4, $6, $7, $9) }

typename_list:
| /* nothing */ { [] }
| typename typename_list { $1 :: $2 }

typename:
| TYPE_UNIT  { Unit }
| TYPE_BOOL  { Bool }
| TYPE_INT   { Int }
| TYPE_FLOAT { Float }
| TYPE_STR   { String }


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
| music     { $1 }
| expr OCTAVE expr {Binop($1, Octave, $3)}
| OCTAVE non_apply non_apply {Binop($2, Octave, $3)}
| expr COLON expr {Binop($1, Zip, $3)}
| expr COMMA expr {Binop($1, Chord, $3)}
| asn_toplevel { $1 }
| expr CONCAT expr { Binop($1, Concat, $3) }
| var_ref DOT_LPAREN expr RPAREN { ArrIdx($1, $3) }
| control { $1 }
| THROW non_apply { Throw($2) }

control:
| IF sep_expr_sep THEN sep_expr_sep ELSE sep_star expr { Conditional($2,$4,$7) }
| BE sep_expr_sep UNLESS sep_expr_sep INWHICHCASE sep_star expr { Conditional($4,$7,$2) }
| FOR sep_star ID_VAR sep_star IN sep_expr_sep DO sep_star expr { For($3,$6,$9) }
| INIT ID_VAR { StructInit($2, []) }
| INIT ID_VAR LBRACE sep_star asn_list sep_star RBRACE { StructInit($2, List.rev $5) }

id_var_list:
| /* nothing */ { [] }
| ID_VAR id_var_list { $1 :: $2 }

stmt_list_plus:
| non_apply { [$1] }
| stmt_list_plus non_apply { $2 :: $1 }

apply:
| ID_FUN args_list { FunApply($1, $2) }

args_list:
| /* nothing */       { [] }
| non_apply args_list { $1 :: $2 }

non_apply:
| var_ref { VarRef($1) }
| LPAREN block RPAREN { $2 } /* we get unit () notation for free (see block) */
| LBRACE stmt_list_plus RBRACE { Arr((List.rev $2), None) }
| LBRACK stmt_list_plus RBRACK { ArrMusic((List.rev $2), None) }
| lit { $1 }
| empty_list  { $1 }

empty_list:
| typename   braces_list  { let rec create_array typ = match typ with 
                              |Array(t) -> Array(create_array t)
                              |_ -> $1
                            in
                              Arr([], Some(create_array $2)) }
| ID_VAR     braces_list  { let rec create_array typ = match typ with 
                              |Array(t) -> Array(create_array t)
                              |_ -> Type($1)
                            in
                              Arr([], Some(create_array $2))} 
| TYPE_INT   BRACKS       { ArrMusic([], Some(Type("pitch"))) }
| TYPE_FLOAT BRACKS       { ArrMusic([], Some(Float)) }
| ID_VAR     BRACKS       { ArrMusic([], Some(Type($1)))}

braces_list:
|braces_list BRACES { (fun f -> Array(f)) $1 } 
|BRACES             { Unit }


sep_expr_sep:
| sep_star expr sep_star { $2 }

lit:
| LIT_BOOL         { LitBool($1) }
| LIT_INT          { LitInt($1) }
| LIT_FLOAT        { LitFloat($1) }
| LIT_STR          { LitStr($1) }
| TILDE            { StructInit("chord", []) }

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
| expr FLAT     { Uniop(Flat, $1)}
| expr SHARP    { Uniop(Sharp, $1)}

var_ref:
| ID_VAR { [ $1 ] }
| ID_VAR BLING var_ref { $1 :: $3 }

asn_toplevel:
| asn { $1 }
/* ID_VAR because you can't do `const a$b = 10` */
| CONST ID_VAR ASSIGN expr { Assign([ $2 ], $4, Immutable) }

asn:
| var_ref ASSIGN expr { Assign($1, $3, Mutable) }

asn_list:
| asn { [ $1 ] }
| asn_list sep_plus asn { $3 :: $1 }
