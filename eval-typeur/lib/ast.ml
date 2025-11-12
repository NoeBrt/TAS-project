type pterm =  Var of string
              | Abs of string * pterm
              | App of pterm * pterm
              | N of int
              | Add of pterm * pterm

type ptype = Var of string
            | Arr of ptype * ptype | Nat
