type action = Ast | Sast | Java

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast); ("-s", Sast); ("-j", Java)]
    else Ast in
  let input = open_in Sys.argv.(2) in
  let lexbuf = Lexing.from_channel input in
  let program = Parser.program Scanner.token lexbuf in
  let program_t = Typecheck.check_program program in
  match action with
      Ast -> let listing = Ast.string_of_program program
           in print_string listing
    | Sast -> let listing = Sast.string_of_program_t program_t
    	   in print_string listing
    | Java -> let listing = Translator.to_java program_t Sys.argv(2)
          in print_string listing
