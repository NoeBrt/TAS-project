let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic ; s

let get_content arg = if Sys.file_exists arg then read_file arg else arg

let () =
  let usage_msg =
    "Usage: lambda [-eval|-type] <filename|expression> or -script <filename>"
  in
  if Array.length Sys.argv < 3 then print_endline usage_msg
  else
    match Sys.argv.(1) with
    | "-eval" -> (
        let arg = Sys.argv.(2) in
        try
          let content = get_content arg in
          let terms = Lambda.Parser.parse_prog content in
          List.iter
            (fun term ->
              let result = Lambda.Evaluation.eval term in
              print_endline (Lambda.Printer.print_term result) )
            terms
        with
        | Sys_error msg ->
            Printf.eprintf "File Error: %s\n" msg
        | Lambda.Parser.Parse_err msg ->
            Printf.eprintf "Parse Error: %s\n" msg
        | Failure msg ->
            Printf.eprintf "Evaluation Error: %s\n" msg
        | e ->
            Printf.eprintf "An unexpected error occurred: %s\n"
              (Printexc.to_string e) )
    | "-type" -> (
        let arg = Sys.argv.(2) in
        try
          let content = get_content arg in
          let terms = Lambda.Parser.parse_prog content in
          List.iter
            (fun term ->
              let result = Lambda.Typeur.inference term in
              print_endline result )
            terms
        with
        | Sys_error msg ->
            Printf.eprintf "File Error: %s\n" msg
        | Lambda.Parser.Parse_err msg ->
            Printf.eprintf "Parse Error: %s\n" msg
        | Failure msg ->
            Printf.eprintf "Type Error: %s\n" msg
        | e ->
            Printf.eprintf "An unexpected error occurred: %s\n"
              (Printexc.to_string e) )
    | "-script" -> (
        let filename = Sys.argv.(2) in
        try
          let content = read_file filename in
          let terms = Lambda.Parser.parse_prog content in
          List.iter
            (fun t ->
              print_endline "--------------------------------------------------" ;
              let type_msg = Lambda.Typeur.inference t in
              print_endline type_msg ;
              print_string ">> Evaluation result: " ;
              try
                let v = Lambda.Evaluation.eval t in
                print_endline (Lambda.Printer.print_term v)
              with
              | Failure s ->
                  print_endline ("Runtime Error: " ^ s)
              | e ->
                  print_endline ("Runtime Exception: " ^ Printexc.to_string e) )
            terms ;
          print_endline "--------------------------------------------------"
        with
        | Sys_error msg ->
            Printf.eprintf "File Error: %s\n" msg
        | Lambda.Parser.Parse_err msg ->
            Printf.eprintf "Parse Error: %s\n" msg
        | Failure msg ->
            Printf.eprintf "Evaluation Error: %s\n" msg
        | e ->
            Printf.eprintf "An unexpected error occurred: %s\n"
              (Printexc.to_string e) )
    | _ ->
        print_endline usage_msg
