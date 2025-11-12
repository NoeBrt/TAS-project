open Lambda.Ast
open Lambda.Parser
open Lambda.Printer
open Alcotest


(* === Helper === *)
let show_tokens toks =
  toks
  |> List.map (function
      | LAMBDA -> "LAMBDA"
      | DOT -> "DOT"
      | PLUS -> "PLUS"
      | LP -> "LP"
      | RP -> "RP"
      | VAR x -> "VAR(" ^ x ^ ")"
      | INT n -> "INT(" ^ string_of_int n ^ ")")
  |> String.concat " "

(* === print_term tests === *)
let test_print_simple_lambda_term () =
  let t = Abs ("x", Var "x") in
  check string "simple lambda term" "(λx.x)" (print_term t)

let test_print_application_and_addition () =
  let t = Add (App (Var "f", N 2), N 3) in
  check string "application and addition" "((f 2)+3)" (print_term t)

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
  check string "deeply nested type"
    "((Nat->A)->(B->(Nat->C)))" (print_type t)

(* === lexer tests === *)
let test_lex_simple_expression () =
  let toks = lex "\\x.x + 3" in
  let expected = "LAMBDA VAR(x) DOT VAR(x) PLUS INT(3)" in
  check string "lex simple expression" expected (show_tokens toks)

let test_lex_parentheses_and_numbers () =
  let toks = lex "((\\f.f) 42)" in
  let expected = "LP LP LAMBDA VAR(f) DOT VAR(f) RP INT(42) RP" in
  check string "lex parentheses and numbers" expected (show_tokens toks)

(* === parser tests === *)
let test_parse_identity_function () =
  let t = parsePTERM "\\x.x" in
  check string "identity function"
    "Abs (\"x\", Var \"x\")" (string_of_pterm t)

let test_parse_nested_lambda () =
  let t = parsePTERM "\\x.\\y.(x y)" in
  check string "nested lambda"
    "Abs (\"x\", Abs (\"y\", App (Var \"x\", Var \"y\")))"
    (string_of_pterm t)

let test_parse_addition () =
  let t = parsePTERM "1 + 2 + 3" in
  check string "addition"
    "Add (Add (N 1, N 2), N 3)" (string_of_pterm t)

let test_parse_application () =
  let t = parsePTERM "((\\f.f) (\\x.x))" in
  check string "application"
    "App (Abs (\"f\", Var \"f\"), Abs (\"x\", Var \"x\"))"
    (string_of_pterm t)

let test_parse_complex_expression () =
  let t = parsePTERM "(\\f.\\x.(f x)) 5 + 3" in
  check string "complex expression"
    "(((λf.(λx.(f x))) 5)+3)" (print_term t)

(* === consistency === *)
let test_type_printing_consistency () =
  let t = Arr (Var "X", Arr (Var "Y", Nat)) in
  let s = print_type t in
  check string "type consistency" "(X->(Y->Nat))" s

(* === test list === *)
let () =
  Alcotest.run "Lambda tests"
    [
      ( "print_term",
        [
          test_case "simple lambda" `Quick test_print_simple_lambda_term;
          test_case "application and addition" `Quick test_print_application_and_addition;
        ] );
      ( "print_type",
        [
          test_case "Nat" `Quick test_print_simple_nat;
          test_case "variable type" `Quick test_print_variable_type;
          test_case "arrow simple" `Quick test_print_arrow_type_simple;
          test_case "arrow nested" `Quick test_print_nested_arrow_type;
          test_case "deeply nested" `Quick test_print_deeply_nested_type;
          test_case "consistency" `Quick test_type_printing_consistency;
        ] );
      ( "lexer",
        [
          test_case "simple expression" `Quick test_lex_simple_expression;
          test_case "parentheses & numbers" `Quick test_lex_parentheses_and_numbers;
        ] );
      ( "parser",
        [
          test_case "identity" `Quick test_parse_identity_function;
          test_case "nested lambda" `Quick test_parse_nested_lambda;
          test_case "addition" `Quick test_parse_addition;
          test_case "application" `Quick test_parse_application;
          test_case "complex expression" `Quick test_parse_complex_expression;
        ] );
    ]
