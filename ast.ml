type program = rule list
and rule = Rule of term * goals
and goals = term list
and concrete = string * term list
and term = 
  | Int of int
  | Concrete of concrete
  | Varall
  | Var of string
  | List of term list * term option

(* TODO: change list to the following *)
and plist = 
  | Cons of term * plist
  | Nil of term option