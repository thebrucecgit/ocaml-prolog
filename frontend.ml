open Parser
open Solver
open Utils

let [@warning "-32"] string_of_token  = function
| VARALL -> "VARALL"
| VAR s -> Printf.sprintf "VAR(%s)" s
| UNIFY -> "UNIFY"
| SYM s -> Printf.sprintf "SYM(%s)" s
| RPAREN -> "RPAREN"
| RBRAC -> "RBRAC"
| MID -> "MID"
| LPAREN -> "LPAREN"
| LBRAC -> "LBRAC"
| INT i -> Printf.sprintf "INT(%d)" i
| IF -> "IF"
| EOF -> "EOF"
| END -> "END"
| COMMA -> "COMMA"
| ADD -> "ADD"
| SUB -> "SUB"
| MUL -> "MUL"
| IDIV -> "IDIV"
| IS -> "IS"
| EQ -> "EQ"
| NEQ -> "NEQ"
| GT -> "GT"
| LT -> "LT"
| LEQ -> "LEQ"
| GEQ -> "GEQ"
| DEQ -> "DEQ"


let parse_error (lexbuf: Lexing.lexbuf) = 
  let pos = lexbuf.lex_curr_p in 
  let line = string_of_int (pos.pos_lnum) in 
  let pos = string_of_int ((pos.pos_cnum - pos.pos_bol) + 1) in 
     "Parser error at line " ^ line ^ " position " ^ pos 


let print_result = function
| [] -> print_endline "true."
| result -> List.iter
  (fun (name, value) -> name ^ ": " ^ string_of_term value |> print_endline) result

let init file =
  let file_channel = try open_in file
                with _ -> raise (Error "can't open file ") in
  let lexbuf = Lexing.from_channel file_channel in
  try
    let program = Parser.main Lexer.token lexbuf in
    print_endline (string_of_program program);
    let prompt_query () = 
      (try
        print_string "?- " ;
        let raw_query = read_line () in
        if raw_query = "" then () else
        let query = Parser.query Lexer.token (Lexing.from_string raw_query) in
        print_endline (string_of_clauses query);
        let Cont initial = continuation_map print_result (solve program query) in
        let step = ref initial in
        while true do
          let (_, Cont next) = !step () in
          step := next; 
          ignore (read_line ())
        done
      with
      | Fail -> print_endline "false."
      | Error str -> print_endline str) in
    while true do
      prompt_query ()
    done
  with
  | _ -> print_endline (parse_error lexbuf)

let () = 
  if Array.length Sys.argv < 2 then raise (Error "Expected argument [filename.pl]")
  else let filename = Sys.argv.(1) in
    init filename
