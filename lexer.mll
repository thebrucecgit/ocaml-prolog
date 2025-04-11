{
  open Parser
  open Lexing
  exception Error of string
}

let var_reg_exp = ['A'-'Z']+ ['0'-'9' 'A'-'Z' 'a'-'z' '_' '\'']* 
let sym_reg_exp = ['a'-'z']+ ['0'-'9' 'A'-'Z' 'a'-'z' '_' '\'']* 
let int_reg_exp = ['0'-'9']+

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | '!' { CUT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRAC }
  | ']' { RBRAC }
  | '|' { MID }
  | '.' { END }
  | ":-" { IF }
  | '='  { UNIFY }
  | ',' { COMMA }
  | '_' { VARALL }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | "//" { IDIV }
  | "is" { IS }
  | "<" { LT }
  | ">" { GT }
  | "=<" { LEQ }
  | ">=" { GEQ }
  | "=:=" { EQ }
  | "=/=" { NEQ }
  | "==" { DEQ }
  | int_reg_exp as num { INT (int_of_string num) }
  | var_reg_exp as var { VAR var }
  | sym_reg_exp as sym { SYM sym }
  | eof { EOF }
  | _ { raise (Error ("Lexer : Illegal character " ^ (Char.escaped (lexeme_char lexbuf 0)))) }