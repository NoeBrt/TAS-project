open Ast

(* Eval termes *)
let compteur_level = ref 0

let fresh base =
  incr compteur_level;
  base ^ string_of_int !compteur_level

let rec rename (t : pterm) (old_var : string) (new_var : string) : pterm =
  match t with
  | Var v when v = old_var -> Var new_var
  | Var v -> Var v
  | N n -> N n
  | Add (t1, t2) ->
      Add (rename t1 old_var new_var, rename t2 old_var new_var)
  | App (t1, t2) ->
      App (rename t1 old_var new_var, rename t2 old_var new_var)
  | Abs (x, t1) ->
      if x = old_var then
        Abs (x, t1)
      else
        Abs (x, rename t1 old_var new_var)


let rec alpha_convert (t : pterm) : pterm =
  match t with
  | Var v -> Var v
  | N n -> N n
  | Add (t1, t2) ->
      Add (alpha_convert t1, alpha_convert t2)
  | App (t1, t2) ->
      App (alpha_convert t1, alpha_convert t2)
  | Abs (x, t1) ->
      let x_fresh = fresh x in
      let t1' = rename t1 x x_fresh in
      Abs (x_fresh, alpha_convert t1')


let rec substitute (t : pterm) (v : string) (s : pterm) : pterm =
  match t with
  | Var x when x = v -> s
  | Var _ -> t
  | N _ -> t
  | Add (t1, t2) ->
      Add (substitute t1 v s, substitute t2 v s)
  | App (t1, t2) ->
      App (substitute t1 v s, substitute t2 v s)
  | Abs (x, t1) ->
      if x = v then
        t
      else
        Abs (x, substitute t1 v s)


let rec beta_reduce (t : pterm) : pterm =
  match t with
  | App (Abs (x, t1), t2) -> substitute t1 x t2
  | App (t1, t2) ->
      let t1' = beta_reduce t1 in
      if t1' <> t1 then App (t1', t2)
      else App (t1, beta_reduce t2)

  | Abs (x, t1) -> Abs (x, beta_reduce t1)
  | Add (t1, t2) -> Add (beta_reduce t1, beta_reduce t2)
  | _ -> t


let rec delta_reduce (t : pterm) : pterm =
  match t with
  | Add (N n1, N n2) -> N (n1 + n2)
  | Add (t1, t2) -> Add (delta_reduce t1, delta_reduce t2)
  | App (t1, t2) ->App (delta_reduce t1, delta_reduce t2)
  | Abs (x, t1) -> Abs (x, delta_reduce t1)
  | _ -> t


open Printer

let eval ?(timeout = 100) ?(trace = false) (t : pterm) : pterm =
  let t = alpha_convert t in
  if trace then print_endline ("[0] " ^ print_term t);
  let rec loop t n i =
    if n <= 0 then
      failwith "Evaluation timeout"
    else
      let t' = delta_reduce (beta_reduce t) in
      if t' = t then
        t
      else (
        if trace then print_endline ("[" ^ string_of_int (i + 1) ^ "] " ^ print_term t');
        loop t' (n - 1) (i + 1)
      )
  in
  loop t timeout 0;;


