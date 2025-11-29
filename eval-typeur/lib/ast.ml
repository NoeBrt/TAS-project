type pterm =  Var of string
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

type ptype =
  | Var of string
  | Arr of ptype * ptype
  | Nat
  | List of ptype
  | Forall of string * ptype




type env = (string * ptype) list

type equa = (ptype * ptype) list

type equa_zip = equa * equa