open Ast

let rec print_term(t:pterm) : string =
  match t with
  Var x -> x
  |App (t1,t2) -> "("^print_term(t1)^" "^print_term(t2)^")"
  |Abs (x,t) -> "(λ"^x^"."^print_term(t)^")"
  |N n -> string_of_int n
  |Add (t1,t2) -> "("^print_term(t1)^"+"^print_term(t2)^")"
  |Sub (t1,t2) -> "("^print_term(t1)^"-"^print_term(t2)^")"
  |Cons (t1,t2) -> "("^print_term(t1)^"::"^print_term(t2)^")"
  |Head t1 -> "head "^print_term(t1)
  |Tail t1 -> "tail "^print_term(t1)
  |IfZero (t1,t2,t3) -> "if zero "^print_term(t1)^" then "^print_term(t2)^" else "^print_term(t3)
  |IfEmpty (t1,t2,t3) -> "if empty "^print_term(t1)^" then "^print_term(t2)^" else "^print_term(t3)
  |Fix t1 -> "fix "^print_term(t1)
  |Nil -> "nil"
  |Let (x,t1,t2) -> "let "^x^"="^print_term(t1)^" in "^print_term(t2);;



let rec print_type(t:ptype): string =
  match t with
  Var x ->x
  |Arr (t1,t2) -> "("^print_type(t1)^"->"^print_type(t2)^")"
  |Nat -> "Nat"
  |List t1 -> "List "^print_type(t1)
  |Forall (x,t1) -> "Forall "^x^"."^print_type(t1);;



let rec string_of_pterm (t : pterm) : string =
  match t with
  | Var x -> "Var \"" ^ x ^ "\""
  | Abs (x, t) ->
      "Abs (\"" ^ x ^ "\", " ^ string_of_pterm t ^ ")"
  | App (t1, t2) ->
      "App (" ^ string_of_pterm t1 ^ ", " ^ string_of_pterm t2 ^ ")"
  | N n ->
      "N " ^ string_of_int n
  | Add (t1, t2) ->
      "Add (" ^ string_of_pterm t1 ^ ", " ^ string_of_pterm t2 ^ ")"
  | Sub (t1, t2) ->
      "Sub (" ^ string_of_pterm t1 ^ ", " ^ string_of_pterm t2 ^ ")"
  | Cons (t1, t2) ->
      "Cons (" ^ string_of_pterm t1 ^ ", " ^ string_of_pterm t2 ^ ")"
  | Head t1 ->
      "Head (" ^ string_of_pterm t1 ^ ")"
  | Tail t1 ->
      "Tail (" ^ string_of_pterm t1 ^ ")"
  | IfZero (t1, t2, t3) ->
      "IfZero (" ^ string_of_pterm t1 ^ ", " ^ string_of_pterm t2 ^ ", " ^ string_of_pterm t3 ^ ")"
  | IfEmpty (t1, t2, t3) ->
      "IfEmpty (" ^ string_of_pterm t1 ^ ", " ^ string_of_pterm t2 ^ ", " ^ string_of_pterm t3 ^ ")"
  | Fix t1 ->
      "Fix (" ^ string_of_pterm t1 ^ ")"
  | Nil ->
      "Nil"
  | Let (x, t1, t2) ->
      "Let (\"" ^ x ^ "\", " ^ string_of_pterm t1 ^ ", " ^ string_of_pterm t2 ^ ")";;



