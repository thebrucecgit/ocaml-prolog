%{
  open Ast
%}
%token <int> INT
%token <string> VAR SYM
%token LPAREN RPAREN LBRAC RBRAC MID END IF UNIFY COMMA VARALL EOF
%token ADD SUB MUL IDIV IS GT LT GEQ LEQ EQ NEQ DEQ

%nonassoc IS UNIFY GT LT GEQ LEQ EQ NEQ DEQ
%left ADD SUB
%left MUL IDIV

%start main
%start query
%type <program> main
%type <goals> query


%%

main:
|  rules EOF { $1 }

query:
|  goals END { $1 }

arguments:
| term { [$1] }
| term COMMA arguments { $1 :: $3 }

term:
| INT { Int $1 }
| VARALL { Varall }
| VAR { Var $1 } 
| SYM { Concrete ($1, []) }
| SYM LPAREN arguments RPAREN { Concrete ($1, $3) }
| LBRAC RBRAC { List ([], None) }
| LBRAC arguments RBRAC { List ($2, None) }
| LBRAC arguments MID term RBRAC { List ($2, Some $4) }
| term ADD term { Concrete ("+", [$1; $3]) }
| term SUB term { Concrete ("-", [$1; $3]) }
| term MUL term { Concrete ("*", [$1; $3]) }
| term IDIV term { Concrete ("//", [$1; $3]) }
| term IS term { Concrete ("is", [$1; $3]) }
| term UNIFY term { Concrete ("=", [$1; $3]) }
| term EQ term { Concrete ("=:=", [$1; $3]) }
| term NEQ term { Concrete ("=/=", [$1; $3]) }
| term LT term { Concrete ("<", [$1; $3]) }
| term GT term { Concrete (">", [$1; $3]) }
| term LEQ term { Concrete ("=<", [$1; $3]) }
| term GEQ term { Concrete (">=", [$1; $3]) }
| term DEQ term { Concrete ("==", [$1; $3]) }

goals:
| term { [$1] }
| term COMMA goals { $1 :: $3 }
  
rule:
| term { Rule ($1, []) }
| term IF goals { Rule ($1, $3) }

rules:
| rule END { [$1] }
| rule END rules { $1 :: $3 }

%%
  (* trailer *)