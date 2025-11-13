open Lambda.Parser
open Lambda.Printer
open Lambda.Evaluation

let () =
  let term = parsePTERM "\\x.x" in
  print_endline (print_term term);


  let term2 = parsePTERM "(\\f.\\x.(f(f x)))x" in
  print_endline (print_term (alpha_convert term2));;


let term3 = parsePTERM "10 + (\\x.(x+x))10";;

print_endline (print_term (beta_reduce term3));;

print_endline (print_term (delta_reduce (beta_reduce term3)));;


print_endline (string_of_pterm (beta_reduce term3));;

print_endline (string_of_pterm  (delta_reduce ((delta_reduce (beta_reduce term3)))));;
