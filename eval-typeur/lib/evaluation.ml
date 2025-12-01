open Ast

(* Eval termes *)
let compteur_level = ref 0

let fresh base =
    incr compteur_level;
    base ^ string_of_int !compteur_level


(**
type pterm =	Var of string
                            | Abs of string * pterm
                            | App of pterm * pterm
                            | N of int
                            | Add of pterm * pterm
                            | Sub of pterm * pterm
                            | Cons of pterm * pterm
                            | Head of pterm
                            | Tail of pterm
                            | IfZero of pterm * pterm * pterm
                            | IfEmpty of pterm * pterm * pterm
                            | Fix of pterm
                            | Nil
                            | Let of string * pterm * pterm
*)
let rec rename (t : pterm) (old_var : string) (new_var : string) : pterm =
    match t with
    | Var v when v = old_var -> Var new_var
    | Var v -> Var v
    | N n -> N n
    | Add (t1, t2) ->
            Add (rename t1 old_var new_var, rename t2 old_var new_var)
    | Sub (t1, t2) ->
            Sub (rename t1 old_var new_var, rename t2 old_var new_var)
    | App (t1, t2) ->
            App (rename t1 old_var new_var, rename t2 old_var new_var)
    | Abs (x, t1) ->
            if x = old_var then
                Abs (x, t1)
            else
                Abs (x, rename t1 old_var new_var)
    | Cons (t1, t2) ->
            Cons (rename t1 old_var new_var, rename t2 old_var new_var)
    | Head t1 ->
            Head (rename t1 old_var new_var)
    | Tail t1 ->
            Tail (rename t1 old_var new_var)
    | IfZero (t1, t2, t3) ->
            IfZero (rename t1 old_var new_var, rename t2 old_var new_var, rename t3 old_var new_var)
    | IfEmpty (t1, t2, t3) ->
            IfEmpty (rename t1 old_var new_var, rename t2 old_var new_var, rename t3 old_var new_var)
    | Fix t1 ->
            Fix (rename t1 old_var new_var)
    | Nil -> Nil
    | Let (x, t1, t2) ->
            let t1' = rename t1 old_var new_var in
            if x = old_var then
                Let (x, t1', t2)
            else
                Let (x, t1', rename t2 old_var new_var)
    | Unit -> Unit
    | Loc n -> Loc n
    | Ref t -> Ref (rename t old_var new_var)
    | Deref t -> Deref (rename t old_var new_var)
    | Assign (t1, t2) -> Assign (rename t1 old_var new_var, rename t2 old_var new_var)




let rec alpha_convert (t : pterm) : pterm =
    match t with
    | Var v -> Var v
    | N n -> N n
    | Add (t1, t2) ->
            Add (alpha_convert t1, alpha_convert t2)
    | Sub (t1, t2) ->
        Sub (alpha_convert t1, alpha_convert t2)
    | App (t1, t2) ->
            App (alpha_convert t1, alpha_convert t2)
    | Abs (x, t1) ->
            let x_fresh = fresh x in
            let t1' = rename t1 x x_fresh in
            Abs (x_fresh, alpha_convert t1')
    | Cons (t1, t2) ->
            Cons (alpha_convert t1, alpha_convert t2)
    | Head t1 ->
            Head (alpha_convert t1)
    | Tail t1 ->
            Tail (alpha_convert t1)
    | IfZero (t1, t2, t3) ->
            IfZero (alpha_convert t1, alpha_convert t2, alpha_convert t3)
    | IfEmpty (t1, t2, t3) ->
            IfEmpty (alpha_convert t1, alpha_convert t2, alpha_convert t3)
    | Fix t1 ->
            Fix (alpha_convert t1)
    | Nil -> Nil
    | Let (x, t1, t2) ->
            let x_fresh = fresh x in
            let t2' = rename t2 x x_fresh in
            Let (x_fresh, alpha_convert t1, alpha_convert t2')
    | Unit -> Unit
    | Loc n -> Loc n
    | Ref t -> Ref (alpha_convert t)
    | Deref t -> Deref (alpha_convert t)
    | Assign (t1, t2) -> Assign (alpha_convert t1, alpha_convert t2)


module StringSet = Set.Make(String)

(* Collects Free Variables: vars that are used but not defined inside the term *)
let rec fv (t : pterm) : StringSet.t =
    match t with
    | Var x -> StringSet.singleton x
    | Abs (x, t1) -> StringSet.remove x (fv t1)
    | Let (x, t1, t2) -> StringSet.union (fv t1) (StringSet.remove x (fv t2))
    | App (t1, t2) | Add (t1, t2) | Sub (t1, t2) | Cons (t1, t2)
	| Assign (t1, t2) -> StringSet.union (fv t1) (fv t2)
	| IfZero (t1, t2, t3) | IfEmpty (t1, t2, t3) ->
		StringSet.union (fv t1) (StringSet.union (fv t2) (fv t3))
	| Head t1 | Tail t1 | Fix t1 | Ref t1 | Deref t1 -> fv t1
	| N _ | Nil | Unit | Loc _ -> StringSet.empty

(* Collects Bound Variables: vars that are defined (Abs/Let) inside the term *)
let rec bv (t : pterm) : StringSet.t =
    match t with
    | Var _ | N _ | Nil | Unit | Loc _ -> StringSet.empty
    | Abs (x, t1) -> StringSet.add x (bv t1)
    | Let (x, t1, t2) -> StringSet.add x (StringSet.union (bv t1) (bv t2))
    | App (t1, t2) | Add (t1, t2) | Sub (t1, t2) | Cons (t1, t2)
    | Assign (t1, t2) -> StringSet.union (bv t1) (bv t2)
    | IfZero (t1, t2, t3) | IfEmpty (t1, t2, t3) ->
    StringSet.union (bv t1) (StringSet.union (bv t2) (bv t3))
    | Head t1 | Tail t1 | Fix t1 | Ref t1 | Deref t1 -> bv t1


let rec alpha_convert_with_set (t : pterm) (forbidden : StringSet.t) : pterm =
  match t with
  | Var x -> Var x
  | Abs (x, t1) ->
      if StringSet.mem x forbidden then
        let x_new = fresh x in
        let t1_renamed = rename t1 x x_new in
        Abs (x_new, alpha_convert_with_set t1_renamed (StringSet.add x_new forbidden))
      else
        Abs (x, alpha_convert_with_set t1 (StringSet.add x forbidden))
  | Let (x, t1, t2) ->
      let t1' = alpha_convert_with_set t1 forbidden in
      if StringSet.mem x forbidden then
        let x_new = fresh x in
        let t2_renamed = rename t2 x x_new in
        Let (x_new, t1', alpha_convert_with_set t2_renamed (StringSet.add x_new forbidden))
      else
        Let (x, t1', alpha_convert_with_set t2 (StringSet.add x forbidden))
  | App (t1, t2) ->
      App (alpha_convert_with_set t1 forbidden, alpha_convert_with_set t2 forbidden)
  | Add (t1, t2) ->
      Add (alpha_convert_with_set t1 forbidden, alpha_convert_with_set t2 forbidden)
  | Sub (t1, t2) ->
      Sub (alpha_convert_with_set t1 forbidden, alpha_convert_with_set t2 forbidden)
  | Cons (t1, t2) ->
      Cons (alpha_convert_with_set t1 forbidden, alpha_convert_with_set t2 forbidden)
  | Head t1 ->
      Head (alpha_convert_with_set t1 forbidden)
  | Tail t1 ->
      Tail (alpha_convert_with_set t1 forbidden)
  | IfZero (t1, t2, t3) ->
      IfZero (alpha_convert_with_set t1 forbidden,
              alpha_convert_with_set t2 forbidden,
              alpha_convert_with_set t3 forbidden)
  | IfEmpty (t1, t2, t3) ->
      IfEmpty (alpha_convert_with_set t1 forbidden,
               alpha_convert_with_set t2 forbidden,
               alpha_convert_with_set t3 forbidden)
  | Fix t1 ->
      Fix (alpha_convert_with_set t1 forbidden)
  | Ref t1 -> Ref (alpha_convert_with_set t1 forbidden)
  | Deref t1 -> Deref (alpha_convert_with_set t1 forbidden)
  | Assign (t1, t2) ->
      Assign (alpha_convert_with_set t1 forbidden, alpha_convert_with_set t2 forbidden)
  | _ -> t

let rec substitute (t : pterm) (v : string) (s : pterm) : pterm =
    match t with
    | Var x when x = v -> s
    | Var _ -> t
    | N _ -> t
    | Add (t1, t2) ->
            Add (substitute t1 v s, substitute t2 v s)
    | Sub (t1, t2) ->
        Sub (substitute t1 v s, substitute t2 v s)

    | App (t1, t2) ->
            App (substitute t1 v s, substitute t2 v s)
    | Abs (x, t1) ->
            if x = v then
                t
            else
                Abs (x, substitute t1 v s)
    | Cons (t1, t2) ->
            Cons (substitute t1 v s, substitute t2 v s)
    | Head t1 ->
            Head (substitute t1 v s)
    | Tail t1 ->
            Tail (substitute t1 v s)
    | IfZero (t1, t2, t3) ->
            IfZero (substitute t1 v s, substitute t2 v s, substitute t3 v s)
    | IfEmpty (t1, t2, t3) ->
            IfEmpty (substitute t1 v s, substitute t2 v s, substitute t3 v s)
    | Fix t1 ->
            Fix (substitute t1 v s)
    | Nil -> Nil
    | Let (x, t1, t2) ->
            let t1' = substitute t1 v s in
            if x = v then
                Let (x, t1', t2)
            else
                Let (x, t1', substitute t2 v s)
    | Unit -> Unit
    | Loc n -> Loc n
    | Ref t -> Ref (substitute t v s)
    | Deref t -> Deref (substitute t v s)
    | Assign (t1, t2) -> Assign (substitute t1 v s, substitute t2 v s)


let rec beta_reduce (t : pterm) : pterm =
    match t with
    | App (Abs (x, t1), t2) -> substitute t1 x t2
    | App (t1, t2) ->
            let t1' = beta_reduce t1 in
            if t1' <> t1 then App (t1', t2)
            else App (t1, beta_reduce t2)

    | Abs (x, t1) -> Abs (x, beta_reduce t1)
    | Add (t1, t2) -> Add (beta_reduce t1, beta_reduce t2)
    | Sub (t1, t2) -> Sub (beta_reduce t1, beta_reduce t2)
    | Cons (t1, t2) -> Cons (beta_reduce t1, beta_reduce t2)
    | Head t1 -> Head (beta_reduce t1)
    | Tail t1 -> Tail (beta_reduce t1)
    | IfZero (t1, t2, t3) ->
            IfZero (beta_reduce t1, beta_reduce t2, beta_reduce t3)
    | IfEmpty (t1, t2, t3) ->
            IfEmpty (beta_reduce t1, beta_reduce t2, beta_reduce t3)
    | Fix t1 -> Fix (beta_reduce t1)
    | Let (x, e1, e2) ->
            Let (x, beta_reduce e1, beta_reduce e2)
    | Ref t -> Ref (beta_reduce t)
    | Deref t -> Deref (beta_reduce t)
    | Assign (t1, t2) -> Assign (beta_reduce t1, beta_reduce t2)
    | _ -> t

    let rec is_value t =
    match t with
    | N _ -> true
    | Abs _ -> true
    | Nil -> true
    | Cons (v1, v2) when is_value v1 && is_value v2 -> true
    | Unit -> true
    | Loc _ -> true
    | _ -> false

(* Store for imperative features *)
let store : (int * pterm) list ref = ref []
let next_loc = ref 0
let new_loc () =
    let l = !next_loc in
    incr next_loc;
    l

let rec delta_reduce t =
    match t with

    (* ---- Arithmetic ---- *)
    | Add (N n1, N n2) -> N (n1 + n2)
    | Add (t1, t2) ->
            let t1' = delta_reduce t1 in
            Add (t1', if t1' <> t1 then t2 else delta_reduce t2)

    | Sub (N n1, N n2) -> N (n1 - n2)
    | Sub (t1, t2) ->
            let t1' = delta_reduce t1 in
            Sub (t1', if t1' <> t1 then t2 else delta_reduce t2)

    (* ---- IfZero ---- *)
    | IfZero (N 0, t2, _) -> delta_reduce t2
    | IfZero (N _, _, t3) -> delta_reduce t3
    | IfZero (t1, t2, t3) ->
            let t1' = delta_reduce t1 in
            IfZero (t1', t2, t3)

    (* ---- IfEmpty ---- *)
    | IfEmpty (Nil, t2, _) -> delta_reduce t2
    | IfEmpty (Cons _, _, t3) -> delta_reduce t3
    | IfEmpty (t1, t2, t3) ->
            let t1' = delta_reduce t1 in
            IfEmpty (t1', t2, t3)

    (* ---- Lists ---- *)
    | Head (Cons (v, _)) when is_value v -> v
    | Head t1 -> Head (delta_reduce t1)

    | Tail (Cons (_, vs)) when is_value vs -> vs
    | Tail t1 -> Tail (delta_reduce t1)

    (* ---- Fix ---- *)
    | Fix v when is_value v ->
            App (v, Fix v)
    | Fix t1 ->
            Fix (delta_reduce t1)

    (* ---- Contextual recursion ---- *)
    | App (t1, t2) ->
            App (delta_reduce t1, delta_reduce t2)

    | Abs (x, t1) ->
            Abs (x, delta_reduce t1)

    | Cons (t1, t2) ->
            let t1' = delta_reduce t1 in
            Cons (t1', if t1' <> t1 then t2 else delta_reduce t2)

    | Let (x, v, t2) when is_value v ->
            substitute t2 x v
    | Let (x, t1, t2) ->
            let t1' = delta_reduce t1 in
            Let (x, t1', t2)

    (* ---- Imperative ---- *)
    | Ref v when is_value v ->
            let l = new_loc () in
            store := (l, v) :: !store;
            Loc l
    | Ref t -> Ref (delta_reduce t)

    | Deref (Loc l) ->
            (try List.assoc l !store
        with Not_found -> failwith ("Segmentation fault: loc " ^ string_of_int l ^ " not found"))
    | Deref t -> Deref (delta_reduce t)

    | Assign (Loc l, v) when is_value v ->
            store := (l, v) :: List.remove_assoc l !store;
            Unit
    | Assign (t1, t2) ->
            let t1' = delta_reduce t1 in
            if t1' <> t1 then Assign (t1', t2)
            else Assign (t1, delta_reduce t2)

    | _ -> t



open Printer

let eval ?(timeout = 100) ?(trace = false) (t : pterm) : pterm =
    store := []; (* Reset store *)
    next_loc := 0;
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


