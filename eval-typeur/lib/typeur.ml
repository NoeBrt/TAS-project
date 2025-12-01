open Ast
open Printer


(* type ptype =
  | Var of string
  | Arr of ptype * ptype
  | Nat
  | List of ptype
  | Forall of string * ptype *)



(* générateur de noms frais de variables de types *)
let compteur_var : int ref = ref 0

let nouvelle_var () : string = compteur_var := !compteur_var + 1;
  "T"^(string_of_int !compteur_var)


exception VarPasTrouve

(* cherche le type d'une variable dans un environnement *)


(* vérificateur d'occurence de variables *)
let rec appartient_type (v : string) (t : ptype) : bool =
  match t with
    Var v1 when v1 = v -> true
  | Arr (t1, t2) -> (appartient_type v t1) || (appartient_type v t2)
  | List t1 -> appartient_type v t1
  | Forall (v1, t1) when v1 <> v -> appartient_type v t1
  | Nat -> false
  | _ -> false
(* remplace une variable par un type dans type *)
let rec substitue_type (t : ptype) (v : string) (t0 : ptype) : ptype =
  match t with
  | Var x -> if x = v then t0 else Var x
  | Arr (t1, t2) -> Arr (substitue_type t1 v t0, substitue_type t2 v t0)
  | Nat -> Nat
  | List t1 -> List (substitue_type t1 v t0)
  | Forall (x, t1) ->
      if x = v
      then Forall (x, t1)
      else Forall (x, substitue_type t1 v t0)

let rec instancie (t : ptype) : ptype =
  match t with
  | Forall (v, t1) ->
      let nv = nouvelle_var () in
      instancie (substitue_type t1 v (Var nv))
  | _ -> t


let rec cherche_type (v : string) (e : env) : ptype =
  match e with
    [] -> raise VarPasTrouve
  | (v1, t1)::_ when v1 = v -> instancie t1
  | (_, _):: q -> (cherche_type v q)
(* remplace une variable par un type dans une liste d'équations*)
let substitue_type_partout (e : equa) (v : string) (t0 : ptype) : equa =
  List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e

let rec fv_type (t : ptype) : string list =
  match t with
  | Var v -> [v]
  | Arr (t1, t2) -> fv_type t1 @ fv_type t2
  | List t1 -> fv_type t1
  | Forall (v, t1) -> List.filter (fun x -> x <> v) (fv_type t1)
  | Nat -> []

(* collecte les variables de type libres dans un environnement *)
let fv_type_env (e : env) : string list =
  List.concat (List.map (fun (_, t) -> fv_type t) e)

let generalise (t : ptype) (e : env) : ptype =
  let vars_env = fv_type_env e in
  let vars_t = fv_type t in
  let vars_a_quantifier = List.filter (fun v -> not (List.mem v vars_env)) vars_t in
  List.fold_right (fun v acc -> Forall (v, acc)) vars_a_quantifier t


(* genere des equations de typage à partir d'un terme *)


exception Echec_unif of string

(* zipper d'une liste d'équations *)
type equa_zip = equa * equa

(* rembobine le zipper *)
let rembobine (e : equa_zip) =
  match e with
    ([], _) -> e
  | (c::e1, e2) -> (e1, c::e2)

(* remplace unee variable par un type dans un zipper d'équations *)
let substitue_type_zip (e : equa_zip) (v : string) (t0 : ptype) : equa_zip =
  match e with
    (e1, e2) -> (substitue_type_partout e1 v t0, substitue_type_partout e2 v t0)

(* trouve un type associé à une variable dans un zipper d'équation *)
let rec trouve_but (e : equa_zip) (but : string) =
  match e with
    (_, []) -> raise VarPasTrouve
  | (_, (Var v, t)::_) when v = but -> t
  | (_, (t, Var v)::_) when v = but -> t
  | (e1, c::e2) -> trouve_but (c::e1, e2) but

(* résout un système d'équations *)
let rec unification (e : equa_zip) (but : string) : ptype =
  match e with
    (* on a passé toutes les équations : succes *)
    (_, []) -> (try trouve_but (rembobine e) but with VarPasTrouve -> raise (Echec_unif "but pas trouvé"))
    (* equation avec but : on passe *)
  | (e1, (Var v1, t2)::e2) when v1 = but ->  unification ((Var v1, t2)::e1, e2) but
    (* deux variables : remplacer l'une par l'autre *)
  | (e1, (Var v1, Var v2)::e2) ->  unification (substitue_type_zip (rembobine (e1,e2)) v2 (Var v1)) but
    (* une variable à gauche : vérification d'occurence puis remplacement *)
  | (e1, (Var v1, t2)::e2) ->  if appartient_type v1 t2 then raise (Echec_unif ("occurence de "^ v1 ^" dans "^(print_type t2))) else  unification (substitue_type_zip (rembobine (e1,e2)) v1 t2) but
    (* une variable à droite : vérification d'occurence puis remplacement *)
  | (e1, (t1, Var v2)::e2) ->  if appartient_type v2 t1 then raise (Echec_unif ("occurence de "^ v2 ^" dans " ^(print_type t1))) else  unification (substitue_type_zip (rembobine (e1,e2)) v2 t1) but
    (* types fleche des deux cotes : on decompose  *)
  | (e1, (Arr (t1,t2), Arr (t3, t4))::e2) -> unification (e1, (t1, t3)::(t2, t4)::e2) but
    (* types fleche à gauche pas à droite : echec  *)
  | (_, (Arr (_,_), t3)::_) -> raise (Echec_unif ("type fleche non-unifiable avec "^(print_type t3)))
    (* types fleche à droite pas à gauche : echec  *)
  | (_, (t3, Arr (_,_))::_) -> raise (Echec_unif ("type fleche non-unifiable avec "^(print_type t3)))
    (* types nat des deux cotes : on passe *)
  | (e1, (Nat, Nat)::e2) -> unification (e1, e2) but
  | (e1, (List t1, List t2)::e2) -> unification (e1, (t1, t2)::e2) but
    (* type liste à gauche pas à droite: echec *)
  | (_, (List _, t3):: _) -> raise (Echec_unif ("type liste non-unifiable avec "^(print_type t3)))
    (* types liste à droite pas à gauche: echec *)
  | (_, (t3, List _):: _) -> raise (Echec_unif ("type liste non-unifiable avec "^(print_type t3)))
    (* types forall: instanciation *)
  | (e1, (Forall (v, t1), t2)::e2) ->
      let nv = nouvelle_var () in
      unification (e1, (substitue_type t1 v (Var nv), t2)::e2) but
  | (e1, (t1, Forall (v, t2))::e2) ->
      let nv = nouvelle_var () in
      unification (e1, (t1, substitue_type t2 v (Var nv))::e2) but
    (* les autres cas sont impossibles car t est Var, Arr ou Nat et déjà couverts *)

let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with
    Var v -> let tv : ptype = cherche_type v e in [(ty, tv)]
  | App (t1, t2) -> let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Arr (Var nv, ty)) e in
      let eq2 : equa = genere_equa t2 (Var nv) e in
      eq1 @ eq2
  | Abs (x, t) ->
      let nv1 : string = nouvelle_var ()
      and nv2 : string = nouvelle_var () in
      (ty, Arr (Var nv1, Var nv2))::(genere_equa t (Var nv2) ((x, Var nv1)::e))
  | N _ -> [(ty, Nat)]
  | Add (t1, t2) -> let eq1 : equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 Nat e in
      (ty, Nat)::(eq1 @ eq2)
  | Sub (t1, t2) -> let eq1 : equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 Nat e in
      (ty, Nat)::(eq1 @ eq2)
  | Cons (t1, t2) ->
      let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Var nv) e in
      let eq2 : equa = genere_equa t2 (List (Var nv)) e in
      (ty, List (Var nv))::(eq1 @ eq2)
  | Head t1 ->
      let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (List (Var nv)) e in
      (ty, Var nv)::eq1
  | Tail t1 ->
      let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (List (Var nv)) e in
      (ty, List (Var nv))::eq1
  | IfZero (t1, t2, t3) ->
      let eq1 : equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 ty e in
      let eq3 : equa = genere_equa t3 ty e in
      eq1 @ eq2 @ eq3
  | IfEmpty (t1, t2, t3) ->
      let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (List (Var nv)) e in
      let eq2 : equa = genere_equa t2 ty e in
      let eq3 : equa = genere_equa t3 ty e in
      eq1 @ eq2 @ eq3
  | Fix t1 ->
      let nv1 : string = nouvelle_var ()
      and nv2 : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Arr (Var nv1, Var nv2)) e in
      (ty, Arr (Var nv1, Var nv2))::eq1
  | Nil -> (ty, List (Var (nouvelle_var ())))::[]
  | Let (x, t1, t2) ->
      (* 1. on génère des équations pour t1 avec un but frais *)
      let nv1 : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Var nv1) e in
      (* 2. on unifie pour obtenir le type de t1 *)
      let ty1 : ptype = unification ([], eq1) nv1 in
      (* 3. on généralise éventuellement par rapport à l'environnement *)
      let ty1_gen : ptype = generalise ty1 e in
      (* 4. on génère les équations pour t2 dans l'env étendu *)
      genere_equa t2 ty ((x, ty1_gen)::e)


let inference_env (t : pterm) (env : env) : ptype =
  let e : equa_zip = ([], genere_equa t (Var "but") env) in
  unification e "but"
(* enchaine generation d'equation et unification *)
let inference (t : pterm) : string =
  try
    let res = inference_env t [] in
    (print_term t)^" ***TYPABLE*** avec le type "^(print_type res)
  with
  | Echec_unif bla -> (print_term t)^" ***PAS TYPABLE*** : "^bla