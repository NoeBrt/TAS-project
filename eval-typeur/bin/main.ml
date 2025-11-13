open Lambda.Parser
open Lambda.Printer
open Lambda.Evaluation

let term = parsePTERM "\\x.x";;

print_endline (print_term term);;


let term2 = parsePTERM "(\\f.\\x.(f(f x)))x";;

print_endline (print_term (alpha_convert term2));;


let term3 = parsePTERM "10 + (\\x.(x+x))10";;

print_endline (print_term (beta_reduce term3));;

print_endline (print_term (delta_reduce (beta_reduce term3)));;


print_endline (string_of_pterm (beta_reduce term3));;

print_endline (string_of_pterm  (delta_reduce ((delta_reduce (beta_reduce term3)))));;

print_endline "\n\n---- EVAL ---- (\\f.\\x.(f(f x)))x ---------\n\n";;
print_endline (print_term (eval term2));;

let term4 = parsePTERM "(\\x.(\\y.(y + x))) 4 5 ";;

print_endline (print_term (beta_reduce term4));;

print_endline (print_term (beta_reduce (beta_reduce term4)));;

print_endline (print_term (delta_reduce (beta_reduce (beta_reduce term4))));;

print_endline (print_term (eval term4));;

let term5 = parsePTERM "((\\a. \\b. \\c. c (a b)) (\\x. x)) (\\y. y+1) 3";;

print_endline (print_term (eval ~trace:true term5));;