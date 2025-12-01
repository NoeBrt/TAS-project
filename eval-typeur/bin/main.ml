open Lambda.Parser
open Lambda.Printer
open Lambda.Evaluation
open Lambda.Ast
open Lambda.Typeur

let term = parsePTERM "\\x.x" ;;

print_endline (print_term term) ;;

let term2 = parsePTERM "(\\f.\\x.(f(f x)))x" ;;

print_endline (print_term (alpha_convert term2)) ;;

let term3 = parsePTERM "10 + (\\x.(x+x))10" ;;

print_endline (print_term (beta_reduce term3)) ;;

print_endline (print_term (delta_reduce (beta_reduce term3))) ;;

print_endline (string_of_pterm (beta_reduce term3)) ;;

print_endline
  (string_of_pterm (delta_reduce (delta_reduce (beta_reduce term3))))
;;

print_endline "\n\n---- EVAL ---- (\\f.\\x.(f(f x)))x ---------\n\n" ;;

print_endline (print_term (eval term2)) ;;

let term4 = parsePTERM "(\\x.(\\y.(y + x))) 4 5 " ;;

print_endline (print_term (beta_reduce term4)) ;;

print_endline (print_term (beta_reduce (beta_reduce term4))) ;;

print_endline (print_term (delta_reduce (beta_reduce (beta_reduce term4)))) ;;

print_endline (print_term (eval term4)) ;;

let term5 =
  parsePTERM "((\\x.(\\y.(y + x))) 4)  ((\\n.(\\f.(\\x.(f (n f x))))) 5)"
;;

print_endline (print_term (eval ~trace:true term5)) ;;

(* ================= LIST TESTS ================= *)

print_endline "\n\n---- LISTS ----\n" ;
let l1 = parsePTERM "1 :: (2 :: (3 :: nil))" in
print_endline ("Parsed list: " ^ print_term l1) ;
print_endline ("Evaluated list: " ^ print_term (eval l1)) ;
let hd = parsePTERM "head (1 :: (2 :: nil))" in
print_endline ("Head: " ^ print_term (eval hd)) ;
let tl = parsePTERM "tail (1 :: (2 :: nil))" in
print_endline ("Tail: " ^ print_term (eval tl)) ;
let ife1 = parsePTERM "if empty [] then 42 else 0" in
print_endline ("IfEmpty []: " ^ print_term (eval ife1)) ;
let ife2 = parsePTERM "if empty (1 :: nil) then 42 else 0" in
print_endline ("IfEmpty (1 :: nil): " ^ print_term (eval ife2))
;;

(* ================= IFZERO TESTS ================= *)

print_endline "\n\n---- IFZERO ----\n" ;
let iz1 = parsePTERM "if zero 0 then 99 else 100" in
print_endline (print_term (eval iz1)) ;
let iz2 = parsePTERM "if zero 5 then 99 else 100" in
print_endline (print_term (eval iz2)) ;
(* ================= FIX TESTS ================= *)
print_endline "\n\n---- FIX ----\n" ;
(* fix (\f. \n. if zero n then 1 else (n * f (n-1))) *)
let fact =
  parsePTERM "fix (\\f. \\n. if zero n then 1 else ((n) + (f (n-1))))"
in
let fact5 = App (fact, N 5) in
print_endline ("Factorial-like (with +): " ^ print_term (eval fact5)) ;
(* ================= LET TESTS ================= *)
print_endline "\n\n---- LET ----\n" ;
let l0 = parsePTERM "let x = 10 in x + 1" in
print_endline (print_term (eval l0)) ;
let l1 = parsePTERM "let x = 5 in let y = 7 in x + y" in
print_endline (print_term (eval l1)) ;
let l2 = parsePTERM "let x = (1 :: (2 :: nil)) in head x" in
print_endline (print_term (eval l2))
;;

let ex_id : pterm = Abs ("x", Var "x")

let inf_ex_id : string = inference ex_id

let ex_k : pterm = Abs ("x", Abs ("y", Var "x"))

let inf_ex_k : string = inference ex_k

let ex_s : pterm =
  Abs
    ( "x"
    , Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z"))))
    )

let inf_ex_s : string = inference ex_s

let ex_nat1 : pterm = App (Abs ("x", Add (Var "x", N 1)), N 3)

let inf_ex_nat1 : string = inference ex_nat1

let ex_nat2 : pterm = Abs ("x", Add (Var "x", Var "x"))

let inf_ex_nat2 : string = inference ex_nat2

let ex_omega : pterm =
  App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))

let inf_ex_omega : string = inference ex_omega

let ex_nat3 : pterm = App (ex_nat2, ex_id)

let inf_ex_nat3 : string = inference ex_nat3

let () =
  print_endline "======================" ;
  print_endline inf_ex_id ;
  print_endline "======================" ;
  print_endline inf_ex_k ;
  print_endline "======================" ;
  print_endline inf_ex_s ;
  print_endline "======================" ;
  print_endline inf_ex_omega ;
  print_endline "======================" ;
  print_endline inf_ex_nat1 ;
  print_endline "======================" ;
  print_endline inf_ex_nat2 ;
  print_endline "======================" ;
  print_endline inf_ex_nat3
