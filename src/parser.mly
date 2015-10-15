%{ open Ast %}

%token SEP
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE DOT_LPAREN CONCAT
%token PLUS MINUS TIMES DIVIDE MOD
%token ASSIGN
%token EOF

%token <bytes> ID_VAR
%token <bytes> ID_FUN

%token <bool> LIT_BOOL
%token <int> LIT_INT
%token <float> LIT_FLOAT
%token <bytes> LIT_STR

%left SEP
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE MOD

%start expr
%type < Ast.expr> expr

%%

expr:
| apply     { $1 }
| non_apply { $1 }
| arith { $1 }
| ID_VAR DOT_LPAREN expr RPAREN { ArrIdx($1, $3) }
| LBRACK stmt_list RBRACK { Arr(List.rev $2) }
| LBRACE stmt_list RBRACE { ArrMusic(List.rev $2) }

stmt_list:
| /* nothing */ { [] }
| stmt_list non_apply { $2 :: $1 }

apply:
| apply     non_apply { FunApply($1, $2) }
| non_apply non_apply { FunApply($1, $2) }

non_apply:
| LPAREN expr RPAREN { $2 }
| lit                { $1 }
| ID_VAR             { IdVar($1) }
| ID_FUN             { IdFun($1) }

lit:
| LIT_BOOL         { LitBool($1) }
| LIT_INT          { LitInt($1) }
| LIT_FLOAT        { LitFloat($1) }
| LIT_STR          { LitStr($1) }

arith:
| expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr MOD    expr { Binop($1, Mod, $3) }
