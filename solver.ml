open Ast
open Utils

module StringMap = Map.Make(String)
type varMap = string StringMap.t
type termMap = term StringMap.t
type maps = varMap * termMap

exception Error of string
exception Fail

let trace = false

let convert_rule (Rule (term, clauses)) =
  let id = string_of_int (Random.int 1000000) in
  let rec alpha_convert = function
  | Var name -> Var ("_" ^ id ^ name)
  | Concrete (name, args) -> Concrete (name, List.map alpha_convert args)
  | List (list, tail) -> 
    List (List.map alpha_convert list, Option.map alpha_convert tail) 
  | other -> other in
  Rule (alpha_convert term, List.map alpha_convert clauses)

let rec find_root label map =
  match StringMap.find_opt label map with
  | None -> label
  | Some v when v = label -> v 
  | Some v -> find_root v map
  
let rec unify maps a b : maps =
  match a, b with
  | Varall, _ | _, Varall -> maps
  | Var x, Var y -> 
    let (varSet, terms) = maps in
    let rootX = find_root x varSet in
    let rootY = find_root y varSet in
    (match StringMap.(find_opt rootX terms, find_opt rootY terms) with
    | None, None | None, Some _ -> (StringMap.add rootX rootY varSet, terms)
    | Some _, None              -> (StringMap.add rootY rootX varSet, terms)
    | Some t1, Some t2 -> 
      let (varSet, terms) = unify maps t1 t2 in
      (StringMap.add rootY rootX varSet, terms))    
  | Var x, (List (_, _) | Concrete (_, _) | Int _) -> 
    let concrete = b in
    let (varSet, terms) = maps in
    let root = find_root x varSet in
    (match StringMap.find_opt root terms with
    | None -> (varSet, StringMap.add root concrete terms)
    | Some term -> unify maps term concrete)
  | (List (_, _) | Concrete (_, _) | Int _), Var _ -> unify maps b a
  | Int i, Int j -> 
    if i <> j then raise Fail
    else maps
  | Concrete (aname, aargs), Concrete(bname, bargs) -> 
    if aname <> bname || List.length aargs <> List.length bargs then raise Fail
    else if trace then print_endline ("matched " ^ aname ^ " with " ^ bname) else (); 
    List.fold_left2 unify maps aargs bargs
  | List (ah::at, atail), List (bh::bt, btail) ->
    unify (unify maps ah bh) (List (at, atail)) (List (bt, btail))
  | List ([], Some var1), List ([], Some var2) ->
    unify maps var1 var2
  | List ([], Some var), List (list, tail) ->
    unify maps var (List (list, tail))
  | List (_, _), List ([], Some _) -> unify maps b a
  | List ([], None), List ([], None) -> maps
  | List ([], None), List (_, _) | List (_, _), List ([], None) ->
    raise Fail
  | Concrete (_, _), (Int _ | List (_, _)) 
  | Int _, (List (_, _) | Concrete (_, _))
  | List (_, _), (Int _ | Concrete (_,_)) ->
    raise Fail

let unify_opt maps a b = 
  try Some (unify maps a b)
  with Fail -> None
  
let rec ground_term maps = function
  | Var x -> 
    let (varSet, terms) = maps in
    let root = find_root x varSet in
    (match StringMap.find_opt root terms with
    | None -> Var root
    | Some c -> ground_term maps c)
  | Concrete (name, args) -> 
      Concrete (name, List.map (ground_term maps) args)
  | List (heads, None) -> List (List.map (ground_term maps) heads, None)
  | List (heads, Some tail) ->
    let grounded_heads = List.map (ground_term maps) heads in
    (match ground_term maps tail with
    | List (hd, tl) -> List (grounded_heads @ hd, tl)
    | other -> List (grounded_heads, Some other))
  | x -> x

let ground_rule maps (Rule (head, goals)) =
  Rule (ground_term maps head, List.map (ground_term maps) goals)

let rec reduce = function
  | Int i -> i
  | Concrete ("+", [a; b]) -> (reduce a) + (reduce b)
  | Concrete ("-", [a; b]) -> (reduce a) - (reduce b)
  | Concrete ("*", [a; b]) -> (reduce a) * (reduce b)
  | Concrete ("//", [a; b]) -> (reduce a) / (reduce b)
  | Var x -> raise (Error ("Var " ^ x ^ " is not sufficiently instantiated"))
  | _ -> raise (Error "arithmetic error")


let system_rules maps = 
  let check_int_pair a b op = 
    if op (a |> ground_term maps |> reduce) (b |> ground_term maps |> reduce)
    then maps
    else raise Fail
  in function
  (* cases must be orthogonal, as there is no retry mechanism for system rules *)
  | Concrete ("=", [a; b]) -> unify maps a b
  | Concrete ("<", [a; b]) -> check_int_pair a b (<)
  | Concrete (">", [a; b]) -> check_int_pair a b (>)
  | Concrete ("=<", [a; b]) -> check_int_pair a b (<=)
  | Concrete (">=", [a; b]) -> check_int_pair a b (>=)
  | Concrete ("=:=", [a; b]) -> check_int_pair a b (=)
  | Concrete ("=/=", [a; b]) -> check_int_pair a b (<>)
  | Concrete ("==", [a; b]) -> ignore (unify maps a b); maps
  | Concrete ("is", [Var x; expr]) -> 
    unify maps (Var x) (Int (expr |> ground_term maps |> reduce))
  | _ -> raise Fail


let rec get_variables = function
| Var x -> [x]
| Concrete (_, args) -> List.concat_map get_variables args
| List (list, tail) -> 
  List.concat_map get_variables list @ 
  (match tail with Some term -> get_variables term | None -> [])
| _ -> []

let solve program query =
  let rec try_goals state on_success = function
  | [] -> on_success state
  | goal::tl -> 
    let onNextSuccess state = try_goals state on_success tl in
    search state onNextSuccess goal
  
  and search (maps, Cont on_fail) on_success qterm =
  (* tries to unify input goals; calls `onFail` if one fails; calls `onSuccess` if all succeed *)
    let rec match_rules rules maps qterm = 
      match rules with
      | [] -> 
        (try on_success (system_rules maps qterm, Cont on_fail)
        with Fail -> on_fail ())   (* if no user-defined rules match, try system rules *)
      | rule::tl -> 
        let next_rule () = match_rules tl maps qterm in
        let Rule (head, goals) = convert_rule rule in
        match unify_opt maps head qterm with
        | Some unified -> try_goals (unified, Cont next_rule) on_success goals
        | None -> next_rule ()

    in match_rules program maps qterm
  in

  Random.init 183749;
  let variables = query |> List.concat_map get_variables |> List.sort_uniq compare in
  let empty_maps = StringMap.(empty, empty) in
  let not_found () = raise Fail in
  let extract_variables (maps: maps) = 
    List.map (fun var -> (var, ground_term maps (Var var))) variables in
  let initial = 
    Cont (fun () -> (try_goals [@tailend]) (empty_maps, Cont not_found) Fun.id query) in
  continuation_map extract_variables initial
