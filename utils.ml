open Ast
open Effect
(* open Effect.Deep *)

module StringMap = Map.Make(String)
type varMap = string StringMap.t
type termMap = term StringMap.t
type maps = varMap * termMap

type _ Effect.t += 
  | Return: maps -> unit t
  | ReturnList: (string * term) list -> unit t

let rec string_of_term = function
| Concrete (sym, []) -> sym
| Concrete (sym, x) -> sym ^ "(" ^ (x |> List.map string_of_term |> String.concat ",") ^ ")"
| Varall -> "_"
| Var x -> x
| Int x -> string_of_int x
| List (terms, tl) -> 
  "[" ^ (terms |> List.map string_of_term |> String.concat ",") ^ 
  (match tl with
  | Some term -> "|" ^ string_of_term term
  | None -> "")
  ^ "]"

let string_of_clauses clauses = clauses |> List.map string_of_term |> String.concat ", "

let string_of_rule (Rule (def, clauses)) =
  match clauses with
  | [] -> string_of_term def ^ "."
  | _ -> string_of_term def ^ " :- " ^ (string_of_clauses clauses) ^ "."

let string_of_program (rules : program) : string =
  String.concat "\n" (List.map string_of_rule rules)
    
