open Ast

type tok = LAMBDA | DOT | PLUS | LP | RP | VAR of string | INT of int

let lex s =
  let buf, toks = Buffer.create 10, ref [] in
  let push t = toks := t :: !toks in
  let flush () =
    if Buffer.length buf > 0 then (push (VAR (Buffer.contents buf)); Buffer.clear buf)
  in
  let is_digit c = '0' <= c && c <= '9' in
  let rec loop i =
    if i = String.length s then flush ()
    else match s.[i] with
    | ' ' | '\t' | '\n' -> flush (); loop (i+1)
    | '(' -> flush (); push LP; loop (i+1)
    | ')' -> flush (); push RP; loop (i+1)
    | '.' -> flush (); push DOT; loop (i+1)
    | '+' -> flush (); push PLUS; loop (i+1)
    | '\\' -> flush (); push LAMBDA; loop (i + 1)
    | c when is_digit c ->
        let j = ref i in
        while !j < String.length s && is_digit s.[!j] do incr j done;
        push (INT (int_of_string (String.sub s i (!j - i))));
        loop !j
    | c -> Buffer.add_char buf c; loop (i+1)
  in
  loop 0; List.rev !toks

exception Parse_err of string

let rec term toks =
  match toks with
  | LAMBDA :: VAR x :: DOT :: rest ->
      let t, rest' = term rest in
      (Abs (x, t), rest')
  | _ -> sum toks

and sum toks =
  let t1, rest1 = app toks in
  let rec aux acc toks =
    match toks with
    | PLUS :: rest ->
        let t2, rest' = app rest in
        aux (Add (acc, t2)) rest'
    | _ -> acc, toks
  in
  aux t1 rest1

and app toks =
  let t1, rest1 = atom toks in
  let rec aux acc toks =
    match toks with
    | (VAR _ | INT _ | LP) :: _ as rest ->
        let t2, rest' = atom rest in
        aux (App (acc, t2)) rest'
    | _ -> acc, toks
  in
  aux t1 rest1

and atom = function
  | VAR x :: rest -> Var x, rest
  | INT n :: rest -> N n, rest
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
