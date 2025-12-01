open Ast

type tok =
  | LAMBDA
  | DOT
  | PLUS
  | MINUS
  | CONS
  | LP
  | RP
  | EQUAL
  | LET
  | IN
  | IF
  | ZERO
  | EMPTY
  | THEN
  | ELSE
  | HEAD
  | TAIL
  | FIX
  | NIL
  | UNIT
  | REF
  | DEREF
  | ASSIGN
  | SEMISEMI
  | VAR of string
  | INT of int

let lex s =
  let buf, toks = Buffer.create 10, ref [] in
  let push t = toks := t :: !toks in
  let keyword_or_var str =
    match str with
    | "let" -> LET
    | "in" -> IN
    | "if" -> IF
    | "then" -> THEN
    | "else" -> ELSE
    | "zero" -> ZERO
    | "empty" -> EMPTY
    | "head" -> HEAD
    | "tail" -> TAIL
    | "fix" -> FIX
    | "ref" -> REF
    | _ -> VAR str
  in
  let flush () =
    if Buffer.length buf > 0 then (
      let str = Buffer.contents buf in
      push (keyword_or_var str);
      Buffer.clear buf
    )
  in
  let is_digit c = '0' <= c && c <= '9' in
  let rec loop i =
    if i = String.length s then flush ()
    else match s.[i] with
    | ' ' | '\t' | '\n' -> flush (); loop (i+1)
    | '#' ->
        flush ();
        let rec skip_comment i =
          if i < String.length s && s.[i] <> '\n' then skip_comment (i+1)
          else i
        in
        loop (skip_comment i)
    | '(' ->
        flush ();
        if i + 1 < String.length s && s.[i+1] = ')' then (push UNIT; loop (i+2))
        else (push LP; loop (i+1))
    | ')' -> flush (); push RP; loop (i+1)
    | '.' -> flush (); push DOT; loop (i+1)
    | '+' -> flush (); push PLUS; loop (i+1)
    | '-' -> flush (); push MINUS; loop (i+1)
    | '=' -> flush (); push EQUAL; loop (i+1)
    | '!' -> flush (); push DEREF; loop (i+1)
    | ';' when i + 1 < String.length s && s.[i + 1] = ';' ->
        flush (); push SEMISEMI; loop (i + 2)
    | ':' when i + 1 < String.length s && s.[i + 1] = '=' ->
        flush (); push ASSIGN; loop (i + 2)
    | '\\' -> flush (); push LAMBDA; loop (i + 1)
    | ':' when i + 1 < String.length s && s.[i + 1] = ':' ->
        flush (); push CONS; loop (i + 2)
    | '[' when i + 1 < String.length s && s.[i + 1] = ']' ->
        flush (); push NIL; loop (i + 2)
    | c when is_digit c ->
        if Buffer.length buf > 0 then (
          Buffer.add_char buf c;
          loop (i+1)
        ) else (
          let j = ref i in
          while !j < String.length s && is_digit s.[!j] do incr j done;
          push (INT (int_of_string (String.sub s i (!j - i))));
          loop !j
        )
    | c -> Buffer.add_char buf c; loop (i+1)
  in
  loop 0; List.rev !toks

exception Parse_err of string

let rec term toks =
  match toks with
  | LET :: VAR x :: EQUAL :: rest ->
      let t1, rest1 = term rest in
      (match rest1 with
        | IN :: rest2 ->
          let t2, rest3 = term rest2 in
          (Let (x, t1, t2), rest3)
          | _ -> raise (Parse_err "expected 'in' after let binding"))
  | IF :: ZERO :: rest ->
      let cond, rest1 = term rest in
      (match rest1 with
        | THEN :: rest2 ->
          let t_then, rest3 = term rest2 in
          (match rest3 with
            | ELSE :: rest4 ->
                let t_else, rest5 = term rest4 in
                (IfZero (cond, t_then, t_else), rest5)
            | _ -> raise (Parse_err "expected 'else'"))
      | _ -> raise (Parse_err "expected 'then'"))
    | IF :: EMPTY :: rest ->
      let cond, rest1 = term rest in
      (match rest1 with
        | THEN :: rest2 ->
          let t_then, rest3 = term rest2 in
          (match rest3 with
            | ELSE :: rest4 ->
                let t_else, rest5 = term rest4 in
                (IfEmpty (cond, t_then, t_else), rest5)
            | _ -> raise (Parse_err "expected 'else'"))
      | _ -> raise (Parse_err "expected 'then'"))
  | LAMBDA :: VAR x :: DOT :: rest ->
      let t, rest' = term rest in
      (Abs (x, t), rest')
  | _ -> assign toks

and assign toks =
  let t1, rest1 = cons toks in
  match rest1 with
  | ASSIGN :: rest ->
      let t2, rest' = assign rest in
      (Assign (t1, t2), rest')
  | _ -> t1, rest1

and cons toks =
  let t1, rest1 = sum toks in
  match rest1 with
  | CONS :: rest ->
      let t2, rest' = cons rest in
      (Cons (t1, t2), rest')
  | _ -> t1, rest1

and sum toks =
  let t1, rest1 = app toks in
  let rec aux acc toks =
    match toks with
    | PLUS :: rest ->
        let t2, rest' = app rest in
        aux (Add (acc, t2)) rest'
    | MINUS :: rest ->
        let t2, rest' = app rest in
        aux (Sub (acc, t2)) rest'
    | _ -> acc, toks
  in
  aux t1 rest1

and app toks =
  let t1, rest1 = atom toks in
  let rec aux acc toks =
    match toks with
    | (VAR _ | INT _ | LP | NIL | HEAD | TAIL | FIX | REF | DEREF | UNIT) :: _ as rest ->
        let t2, rest' = atom rest in
        aux (App (acc, t2)) rest'
    | _ -> acc, toks
  in
  aux t1 rest1

and atom = function
  | VAR x :: rest -> Var x, rest
  | INT n :: rest -> N n, rest
  | NIL :: rest -> Nil, rest
  | UNIT :: rest -> Unit, rest
  | REF :: rest ->
      let t, rest' = atom rest in
      (Ref t, rest')
  | DEREF :: rest ->
      let t, rest' = atom rest in
      (Deref t, rest')
  | HEAD :: rest ->
      let t, rest' = atom rest in
      (Head t, rest')
  | TAIL :: rest ->
      let t, rest' = atom rest in
      (Tail t, rest')
    | FIX :: rest ->
      let t, rest' = term rest in
      (Fix t, rest')
  | LP :: rest ->
      let t, rest' = term rest in
      (match rest' with
      | RP :: r -> t, r
      | _ -> raise (Parse_err "missing )"))
  | _ -> raise (Parse_err "bad token")


let parsePTERM (s : string) : pterm =
  let t, rest = term (lex s) in
  match rest with
  | [] -> t
  | _ -> raise (Parse_err "trailing tokens")

let rec terms toks =
  match toks with
  | [] -> []
  | _ ->
    let t, rest = term toks in
    match rest with
    | SEMISEMI :: rest' -> t :: terms rest'
    | [] -> [t]
    | _ -> raise (Parse_err "expected ;; or EOF")

let parse_prog (s : string) : pterm list =
  terms (lex s)
