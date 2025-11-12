open Lambda.Parser
open Lambda.Printer

let () =
  let term = parsePTERM "\\x.x" in
  print_endline (print_term term)
