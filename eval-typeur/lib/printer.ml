open Ast

let rec print_term(t:pterm) : string =
  match t with
  Var x -> x
  |App (t1,t2) -> "("^print_term(t1)^" "^print_term(t2)^")"
  |Abs (x,t) -> "(λ"^x^"."^print_term(t)^")"
  |N n -> string_of_int n
  |Add (t1,t2) -> "("^print_term(t1)^"+"^print_term(t2)^")";;


let rec print_type(t:ptype): string =
  match t with
  Var x ->x
  |Arr (t1,t2) -> "("^print_type(t1)^"->"^print_type(t2)^")"
  |Nat -> "Nat";;

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
      "Add (" ^ string_of_pterm t1 ^ ", " ^ string_of_pterm t2 ^ ")";;



