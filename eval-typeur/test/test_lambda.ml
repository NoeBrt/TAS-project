open Lambda.Ast
open Lambda.Parser
open Lambda.Printer
open Lambda.Evaluation
open Lambda.Typeur
open Alcotest

(* === Helper === *)
let show_tokens toks =
  toks
  |> List.map (function
    | LAMBDA ->
        "LAMBDA"
    | DOT ->
        "DOT"
    | PLUS ->
        "PLUS"
    | MINUS ->
        "MINUS"
    | CONS ->
        "CONS"
    | LP ->
        "LP"
    | RP ->
        "RP"
    | EQUAL ->
        "EQUAL"
    | LET ->
        "LET"
    | IN ->
        "IN"
    | IF ->
        "IF"
    | ZERO ->
        "ZERO"
    | EMPTY ->
        "EMPTY"
    | THEN ->
        "THEN"
    | ELSE ->
        "ELSE"
    | HEAD ->
        "HEAD"
    | TAIL ->
        "TAIL"
    | FIX ->
        "FIX"
    | NIL ->
        "NIL"
    | UNIT ->
        "UNIT"
    | REF ->
        "REF"
    | DEREF ->
        "DEREF"
    | ASSIGN ->
        "ASSIGN"
    | SEMISEMI ->
        "SEMISEMI"
    | VAR x ->
        "VAR(" ^ x ^ ")"
    | INT n ->
        "INT(" ^ string_of_int n ^ ")" )
  |> String.concat " "

(* === print_term tests === *)
let test_print_simple_lambda_term () =
  let t = Abs ("x", Var "x") in
  check string "simple lambda term" "(λx.x)" (print_term t)

let test_print_application_and_addition () =
  let t = Add (App (Var "f", N 2), N 3) in
  check string "application and addition" "((f 2)+3)" (print_term t)

let test_print_subtraction () =
  let t = Sub (N 5, N 3) in
  check string "subtraction" "(5-3)" (print_term t)

let test_print_cons_head_tail () =
  let cons_term = Cons (N 1, Nil) in
  let head_term = Head (Var "xs") in
  let tail_term = Tail (Var "xs") in
  check string "cons" "(1::nil)" (print_term cons_term) ;
  check string "head" "head xs" (print_term head_term) ;
  check string "tail" "tail xs" (print_term tail_term)

let test_print_conditionals_and_fix () =
  let if_zero_term = IfZero (N 0, N 1, N 2) in
  let if_empty_term = IfEmpty (Nil, N 1, N 2) in
  let fix_term = Fix (Abs ("f", Var "f")) in
  check string "if zero" "if zero 0 then 1 else 2" (print_term if_zero_term) ;
  check string "if empty" "if empty nil then 1 else 2"
    (print_term if_empty_term) ;
  check string "fix" "fix (λf.f)" (print_term fix_term)

let test_print_let_binding () =
  let t = Let ("x", N 1, Add (Var "x", N 2)) in
  check string "let binding" "let x=1 in (x+2)" (print_term t)

(* === print_type tests === *)
let test_print_simple_nat () =
  check string "simple Nat type" "Nat" (print_type Nat)

let test_print_variable_type () =
  check string "variable type" "A" (print_type (Var "A"))

let test_print_arrow_type_simple () =
  let t = Arr (Var "X", Nat) in
  check string "simple arrow type" "(X->Nat)" (print_type t)

let test_print_nested_arrow_type () =
  let t = Arr (Var "X", Arr (Nat, Var "Y")) in
  check string "nested arrow type" "(X->(Nat->Y))" (print_type t)

let test_print_deeply_nested_type () =
  let t = Arr (Arr (Nat, Var "A"), Arr (Var "B", Arr (Nat, Var "C"))) in
  check string "deeply nested type" "((Nat->A)->(B->(Nat->C)))" (print_type t)

let test_print_list_type () =
  let t = List (Arr (Nat, Nat)) in
  check string "list type" "List (Nat->Nat)" (print_type t)

let test_print_forall_type () =
  let t = Forall ("X", Arr (Var "X", List (Var "X"))) in
  check string "forall type" "Forall X.(X->List X)" (print_type t)

(* === lexer tests : ensure every token is surfaced with the expected textual tag === *)
let test_lex_simple_expression () =
  let toks = lex "\\x.x + 3" in
  let expected = "LAMBDA VAR(x) DOT VAR(x) PLUS INT(3)" in
  check string "lex simple expression" expected (show_tokens toks)

let test_lex_parentheses_and_numbers () =
  let toks = lex "((\\f.f) 42)" in
  let expected = "LP LP LAMBDA VAR(f) DOT VAR(f) RP INT(42) RP" in
  check string "lex parentheses and numbers" expected (show_tokens toks)

let test_lex_subtraction () =
  let toks = lex "5 - 3" in
  let expected = "INT(5) MINUS INT(3)" in
  check string "lex subtraction : \"INT(5) MINUS INT(3)\"" expected
    (show_tokens toks)

let test_lex_let_binding () =
  let toks = lex "let x = 1 in x" in
  let expected = "LET VAR(x) EQUAL INT(1) IN VAR(x)" in
  check string "lex let : \"LET VAR(x) EQUAL INT(1) IN VAR(x)\"" expected
    (show_tokens toks)

let test_lex_list_and_conditional_ops () =
  let toks = lex "if empty [] then head xs else tail xs" in
  let expected = "IF EMPTY NIL THEN HEAD VAR(xs) ELSE TAIL VAR(xs)" in
  check string
    "lex list/cond : \"IF EMPTY NIL THEN HEAD VAR(xs) ELSE TAIL VAR(xs)\""
    expected (show_tokens toks)

let test_lex_cons_expression () =
  let toks = lex "1 :: []" in
  let expected = "INT(1) CONS NIL" in
  check string "lex cons : \"INT(1) CONS NIL\"" expected (show_tokens toks)

(* === parser tests : parsed AST must reflect extended surface syntax === *)
let test_parse_identity_function () =
  let t = parsePTERM "\\x.x" in
  check string "identity function" "Abs (\"x\", Var \"x\")" (string_of_pterm t)

let test_parse_nested_lambda () =
  let t = parsePTERM "\\x.\\y.(x y)" in
  check string "nested lambda"
    "Abs (\"x\", Abs (\"y\", App (Var \"x\", Var \"y\")))" (string_of_pterm t)

let test_parse_addition () =
  let t = parsePTERM "1 + 2 + 3" in
  check string "addition" "Add (Add (N 1, N 2), N 3)" (string_of_pterm t)

let test_parse_application () =
  let t = parsePTERM "((\\f.f) (\\x.x))" in
  check string "application"
    "App (Abs (\"f\", Var \"f\"), Abs (\"x\", Var \"x\"))" (string_of_pterm t)

let test_parse_complex_expression () =
  let t = parsePTERM "(\\f.\\x.(f x)) 5 + 3" in
  check string "complex expression" "(((λf.(λx.(f x))) 5)+3)" (print_term t)

let test_parse_subtraction () =
  let t = parsePTERM "5 - 3" in
  check string "subtraction parse : \"Sub (N 5, N 3)\"" "Sub (N 5, N 3)"
    (string_of_pterm t)

let test_parse_let_expression () =
  let t = parsePTERM "let x = 1 in x + 2" in
  check string "let parse : \"Let (\"x\", N 1, Add (Var \"x\", N 2))\""
    "Let (\"x\", N 1, Add (Var \"x\", N 2))" (string_of_pterm t)

let test_parse_fix_expression () =
  let t = parsePTERM "fix \\f.f" in
  check string "fix parse : \"Fix (Abs (\"f\", Var \"f\"))\""
    "Fix (Abs (\"f\", Var \"f\"))" (string_of_pterm t)

let test_parse_ifempty_expression () =
  let t = parsePTERM "if empty [] then 1 else 2" in
  check string "if empty parse : \"IfEmpty (Nil, N 1, N 2)\""
    "IfEmpty (Nil, N 1, N 2)" (string_of_pterm t)

let test_parse_cons_expression () =
  let t = parsePTERM "1 :: []" in
  check string "cons parse : \"Cons (N 1, Nil)\"" "Cons (N 1, Nil)"
    (string_of_pterm t)

(* === consistency === *)
let test_type_printing_consistency () =
  let t = Arr (Var "X", Arr (Var "Y", Nat)) in
  let s = print_type t in
  check string "type consistency" "(X->(Y->Nat))" s

(* === evaluation tests === *)
let test_beta_identity_application () =
  let t = parsePTERM "((\\x.x) 5)" in
  let t' = beta_reduce t in
  check string "beta: identity application" "5" (print_term t')

let test_beta_const_function () =
  let t = parsePTERM "((\\x.\\y.x) 7)" in
  let t' = beta_reduce t in
  check string "beta: const function" "(λy.7)" (print_term t')

let test_delta_add_constants () =
  let t = parsePTERM "1 + 2" in
  let t' = delta_reduce t in
  check string "delta: 1+2 -> 3" "3" (print_term t')

let test_twodelta_nested_addition () =
  let t = parsePTERM "(1 + 2) + 3" in
  let t' = delta_reduce (delta_reduce t) in
  check string "delta: (1+2)+3 -> 6" "6" (print_term t')

let test_beta_then_delta_pipeline () =
  let t = parsePTERM "((\\x.x) (1 + 2))" in
  let after_beta = beta_reduce t in
  check string "beta: strips application" "(1+2)" (print_term after_beta) ;
  let after_delta = delta_reduce after_beta in
  check string "delta: reduces to 3" "3" (print_term after_delta)

let eval_exp () =
  let t = parsePTERM "(\\x.(\\y.(y + x))) 4 5 " in
  let after_eval = eval t in
  check string "eval: (\\x.(\\y.(y + x))) 4 5 -> 9" "9" (print_term after_eval)

(* eval timeout on divergent omega term *)
let test_eval_timeout () =
  let omega =
    parsePTERM "((\\x.(\\y.(y + x))) 4)  ((\\n.(\\f.(\\x.(f (n f x))))) 5)"
  in
  check_raises "eval timeout" (Failure "Evaluation timeout") (fun () ->
      ignore (eval ~timeout:2 omega) )

(* eval with trace should return same result as without trace *)
let test_eval_trace_consistency () =
  let t = parsePTERM "1 + 2 + 3" in
  let r_no_trace = eval t in
  let r_trace = eval ~trace:true t in
  check string "trace consistency" (print_term r_no_trace) (print_term r_trace)

(* === typeur tests (inference) === *)
let contains_substring s sub =
  let n = String.length s and m = String.length sub in
  let rec aux i =
    if i + m > n then false
    else if String.sub s i m = sub then true
    else aux (i + 1)
  in
  aux 0

let test_typeur_ex_id () =
  let t = Abs ("x", Var "x") in
  let res = inference t in
  check bool "ex_id contains term" true (contains_substring res "(λx.x)") ;
  check bool "ex_id typable" true (contains_substring res "***TYPABLE***") ;
  check bool "ex_id arrow" true (contains_substring res "->")

let test_typeur_ex_k () =
  let t = Abs ("x", Abs ("y", Var "x")) in
  let res = inference t in
  check bool "ex_k contains term" true (contains_substring res "(λx.(λy.x))") ;
  check bool "ex_k typable" true (contains_substring res "***TYPABLE***") ;
  check bool "ex_k arrow nested" true (contains_substring res "->(")

let test_typeur_ex_s () =
  let t =
    Abs
      ( "x"
      , Abs
          ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z"))))
      )
  in
  let res = inference t in
  check bool "ex_s contains term" true
    (contains_substring res "(λx.(λy.(λz.((x z) (y z)))))") ;
  check bool "ex_s typable" true (contains_substring res "***TYPABLE***") ;
  check bool "ex_s multi arrows" true (contains_substring res "->(")

let test_typeur_ex_omega () =
  let t =
    App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
  in
  let res = inference t in
  check bool "ex_omega contains term" true
    (contains_substring res "((λx.(x x)) (λy.(y y)))") ;
  check bool "ex_omega not typable" true
    (contains_substring res "***PAS TYPABLE***") ;
  check bool "ex_omega occurs check" true
    (contains_substring res "occurence de T") ;
  check bool "ex_omega occurs in" true (contains_substring res "dans (")

let test_typeur_ex_nat1 () =
  let t = App (Abs ("x", Add (Var "x", N 1)), N 3) in
  let res = inference t in
  check string "ex_nat1 exact" "((λx.(x+1)) 3) ***TYPABLE*** avec le type Nat"
    res

let test_typeur_ex_nat2 () =
  let t = Abs ("x", Add (Var "x", Var "x")) in
  let res = inference t in
  check string "ex_nat2 exact"
    "(λx.(x+x)) ***TYPABLE*** avec le type (Nat->Nat)" res

let test_typeur_ex_nat3 () =
  let t = App (Abs ("x", Add (Var "x", Var "x")), Abs ("x", Var "x")) in
  let res = inference t in
  check string "ex_nat3 exact"
    "((λx.(x+x)) (λx.x)) ***PAS TYPABLE*** : type fleche non-unifiable avec Nat"
    res

let test_typeur_let_id () =
  let t = Let ("id", Abs ("x", Var "x"), Var "id") in
  let res = inference t in
  check bool "let_id typable" true (contains_substring res "***TYPABLE***") ;
  check bool "let_id arrow" true (contains_substring res "->")

let test_typeur_let_poly_app () =
  let t = Let ("id", Abs ("x", Var "x"), App (Var "id", Var "id")) in
  let res = inference t in
  check bool "let_poly_app typable" true
    (contains_substring res "***TYPABLE***") ;
  check bool "let_poly_app arrow" true (contains_substring res "->")

let test_typeur_let_poly_reuse () =
  let t =
    Let
      ( "id"
      , Abs ("x", Var "x")
      , Let ("ignore", App (Var "id", N 0), App (Var "id", Var "id")) )
  in
  let res = inference t in
  check bool "let_poly_reuse typable" true
    (contains_substring res "***TYPABLE***")

let test_typeur_fail_mono () =
  let t = Abs ("f", App (Var "f", Var "f")) in
  let res = inference t in
  check bool "fail_mono not typable" true
    (contains_substring res "***PAS TYPABLE***")

(* === Imperative tests === *)
let test_ref_creation () =
  let t = parsePTERM "ref 1" in
  let res = eval t in
  match res with
  | Loc _ ->
      check bool "ref returns loc" true true
  | _ ->
      check bool "ref returns loc" false true

let test_deref () =
  let t = parsePTERM "!(ref 1)" in
  let res = eval t in
  check string "deref" "1" (print_term res)

let test_assign () =
  let t = parsePTERM "let r = ref 1 in r := 2" in
  let res = eval t in
  check string "assign returns unit" "()" (print_term res)

let test_assign_effect () =
  let t = parsePTERM "let r = ref 1 in let _ = (r := 2) in !r" in
  let res = eval t in
  check string "assign effect" "2" (print_term res)

let test_typeur_ref () =
  let t = parsePTERM "ref 1" in
  let res = inference t in
  check bool "ref typable" true (contains_substring res "***TYPABLE***") ;
  check bool "ref type" true (contains_substring res "Ref Nat")

let test_typeur_deref () =
  let t = parsePTERM "!(ref 1)" in
  let res = inference t in
  check bool "deref typable" true (contains_substring res "***TYPABLE***") ;
  check bool "deref type" true (contains_substring res "Nat")

let test_typeur_assign () =
  let t = parsePTERM "let r = ref 1 in r := 2" in
  let res = inference t in
  check bool "assign typable" true (contains_substring res "***TYPABLE***") ;
  check bool "assign type" true (contains_substring res "Unit")

(* === Alpha conversion with set tests === *)
let test_alpha_convert_with_set_no_collision () =
  let t = Lambda.Ast.Abs ("x", (Lambda.Ast.Var "x" : Lambda.Ast.pterm)) in
  let res = alpha_convert_with_set t StringSet.empty in
  check string "no collision" "(λx.x)" (print_term res)

let test_alpha_convert_with_set_collision () =
  let t = Lambda.Ast.Abs ("x", (Lambda.Ast.Var "x" : Lambda.Ast.pterm)) in
  let set = StringSet.singleton "x" in
  let res = alpha_convert_with_set t set in
  match res with
  | Lambda.Ast.Abs (new_x, Lambda.Ast.Var new_v) ->
      check bool "var renamed" true (new_x <> "x") ;
      check string "body var matches binder" new_x new_v
  | _ ->
      fail "Expected Abs"

let test_alpha_convert_with_set_nested_shadowing () =
  let t =
    Lambda.Ast.Abs
      ("x", Lambda.Ast.Abs ("x", (Lambda.Ast.Var "x" : Lambda.Ast.pterm)))
  in
  let res = alpha_convert_with_set t StringSet.empty in
  match res with
  | Lambda.Ast.Abs (x1, Lambda.Ast.Abs (x2, Lambda.Ast.Var x3)) ->
      check string "outer x kept" "x" x1 ;
      check bool "inner x renamed" true (x2 <> "x") ;
      check string "inner body matches inner binder" x2 x3
  | _ ->
      fail "Expected Abs(Abs)"

let test_alpha_convert_with_set_performance () =
  let t0 = Sys.time () in
  let rec make_deep_term n : Lambda.Ast.pterm =
    if n = 0 then Lambda.Ast.Var "x"
    else Lambda.Ast.Abs ("x", make_deep_term (n - 1))
  in
  let depth = 500 in
  let t = make_deep_term depth in
  let _ = alpha_convert_with_set t StringSet.empty in
  let t1 = Sys.time () in
  Printf.printf "Alpha convert depth %d took %f seconds\n" depth (t1 -. t0) ;
  check bool "performance reasonable" true (t1 -. t0 < 5.0)

(* === test list === *)
let () =
  Alcotest.run "Lambda tests"
    [ ( "print_term"
      , [ test_case "simple lambda : \"(λx.x)\"" `Quick
            test_print_simple_lambda_term
        ; test_case "application and addition : \"((f 2)+3)\"" `Quick
            test_print_application_and_addition
        ; test_case "subtraction : \"(5-3)\"" `Quick test_print_subtraction
        ; test_case "cons/head/tail : \"(1::nil) | head xs | tail xs\"" `Quick
            test_print_cons_head_tail
        ; test_case
            "conditionals and fix : \"if zero 0 then 1 else 2 | if empty nil \
             then 1 else 2 | fix (λf.f)\""
            `Quick test_print_conditionals_and_fix
        ; test_case "let binding : \"let x=1 in (x+2)\"" `Quick
            test_print_let_binding ] )
    ; ( "print_type"
      , [ test_case "Nat : \"Nat\"" `Quick test_print_simple_nat
        ; test_case "variable type : \"A\"" `Quick test_print_variable_type
        ; test_case "arrow simple : \"(X->Nat)\"" `Quick
            test_print_arrow_type_simple
        ; test_case "arrow nested : \"(X->(Nat->Y))\"" `Quick
            test_print_nested_arrow_type
        ; test_case "deeply nested : \"((Nat->A)->(B->(Nat->C)))\"" `Quick
            test_print_deeply_nested_type
        ; test_case "list : \"List (Nat->Nat)\"" `Quick test_print_list_type
        ; test_case "forall : \"Forall X.(X->List X)\"" `Quick
            test_print_forall_type
        ; test_case "consistency" `Quick test_type_printing_consistency ] )
    ; ( "lexer"
      , [ test_case
            "simple expression : \"LAMBDA VAR(x) DOT VAR(x) PLUS INT(3)\""
            `Quick test_lex_simple_expression
        ; test_case
            "parentheses & numbers : \"LP LP LAMBDA VAR(f) DOT VAR(f) RP \
             INT(42) RP\""
            `Quick test_lex_parentheses_and_numbers
        ; test_case "subtraction : \"INT(5) MINUS INT(3)\"" `Quick
            test_lex_subtraction
        ; test_case "let binding : \"LET VAR(x) EQUAL INT(1) IN VAR(x)\"" `Quick
            test_lex_let_binding
        ; test_case
            "list & cond : \"IF EMPTY NIL THEN HEAD VAR(xs) ELSE TAIL VAR(xs)\""
            `Quick test_lex_list_and_conditional_ops
        ; test_case "cons : \"INT(1) CONS NIL\"" `Quick test_lex_cons_expression
        ] )
    ; ( "parser"
      , [ test_case "identity : \"Abs (\"x\", Var \"x\")\"" `Quick
            test_parse_identity_function
        ; test_case
            "nested lambda : \"Abs (\"x\", Abs (\"y\", App (Var \"x\", Var \
             \"y\")))\""
            `Quick test_parse_nested_lambda
        ; test_case "addition : \"Add (Add (N 1, N 2), N 3)\"" `Quick
            test_parse_addition
        ; test_case "subtraction : \"Sub (N 5, N 3)\"" `Quick
            test_parse_subtraction
        ; test_case
            "application : \"App (Abs (\"f\", Var \"f\"), Abs (\"x\", Var \
             \"x\"))\""
            `Quick test_parse_application
        ; test_case "complex expression : \"(((λf.(λx.(f x))) 5)+3)\"" `Quick
            test_parse_complex_expression
        ; test_case "let : \"Let (\"x\", N 1, Add (Var \"x\", N 2))\"" `Quick
            test_parse_let_expression
        ; test_case "fix : \"Fix (Abs (\"f\", Var \"f\"))\"" `Quick
            test_parse_fix_expression
        ; test_case "if empty : \"IfEmpty (Nil, N 1, N 2)\"" `Quick
            test_parse_ifempty_expression
        ; test_case "cons : \"Cons (N 1, Nil)\"" `Quick
            test_parse_cons_expression ] )
    ; ( "evaluation"
      , [ test_case "beta: identity application" `Quick
            test_beta_identity_application
        ; test_case "beta: const function" `Quick test_beta_const_function
        ; test_case "delta: 1+2 -> 3" `Quick test_delta_add_constants
        ; test_case "delta: nested addition -> 6" `Quick
            test_twodelta_nested_addition
        ; test_case "beta then delta pipeline" `Quick
            test_beta_then_delta_pipeline
        ; test_case "eval expression " `Quick eval_exp
        ; test_case "eval timeout: (\\x.(\\y.(y + x))) 4 5 -> 9" `Quick
            test_eval_timeout
        ; test_case "eval trace consistency" `Quick test_eval_trace_consistency
        ] )
    ; ( "typeur"
      , [ test_case "ex_id" `Quick test_typeur_ex_id
        ; test_case "ex_k" `Quick test_typeur_ex_k
        ; test_case "ex_s" `Quick test_typeur_ex_s
        ; test_case "ex_omega" `Quick test_typeur_ex_omega
        ; test_case "ex_nat1" `Quick test_typeur_ex_nat1
        ; test_case "ex_nat2" `Quick test_typeur_ex_nat2
        ; test_case "ex_nat3" `Quick test_typeur_ex_nat3
        ; test_case "let_id" `Quick test_typeur_let_id
        ; test_case "let_poly_app" `Quick test_typeur_let_poly_app
        ; test_case "let_poly_reuse" `Quick test_typeur_let_poly_reuse
        ; test_case "fail_mono" `Quick test_typeur_fail_mono
        ; test_case "ref creation" `Quick test_ref_creation
        ; test_case "deref" `Quick test_deref
        ; test_case "assign" `Quick test_assign
        ; test_case "assign effect" `Quick test_assign_effect
        ; test_case "typeur ref" `Quick test_typeur_ref
        ; test_case "typeur deref" `Quick test_typeur_deref
        ; test_case "typeur assign" `Quick test_typeur_assign ] )
    ; ( "alpha_convert_with_set"
      , [ test_case "no collision" `Quick
            test_alpha_convert_with_set_no_collision
        ; test_case "collision" `Quick test_alpha_convert_with_set_collision
        ; test_case "nested shadowing" `Quick
            test_alpha_convert_with_set_nested_shadowing
        ; test_case "performance" `Quick test_alpha_convert_with_set_performance
        ] ) ]
