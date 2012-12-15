open Unix

type action = Ast | Sast | Java | Class | Help | Version

let version =
  "Stint version 1.0 \"12/18/12\" \n "

let usage =
  "Usage: ./stint <option> <source file>\n where possible options include: \n" ^
    "        -a file.sti       (Output the AST of source file)\n" ^
    "        -s file.sti       (Output the SAST of source file)\n" ^
    "        -j file.sti       (Compile source code to .java file)\n" ^
    "        -c file.sti       (Compile source code to .java and .class files)\n" ^
    "        -help             (Print a synopsis of standard options )\n" ^
    "        -version          (Display version information)\n"
let _ =
  let action = 
    if Array.length Sys.argv > 1 then
      (match Sys.argv.(1) with
          "-a" -> if Array.length Sys.argv == 3 then Ast else Help
        | "-s" -> if (Array.length Sys.argv == 3) then Sast else Help
        | "-j" -> if Array.length Sys.argv == 3 then Java else Help
        | "-c" -> if Array.length Sys.argv == 3 then Class else Help
        | "-v" -> Version
        |   _  -> Help )
    else Help in

    match action with
      Help -> print_endline (usage)
    | Version -> print_endline (version)
    | (Ast | Sast | Java | Class) ->
      let fname = Sys.argv.(2) in 
      let index = (if String.contains fname '.' then String.rindex fname '.' else 0 ) in 
      let suffix = String.sub fname index 4 in
      if not (suffix = ".sti") then raise (Failure ("Invalid type of source file.")) 
      else 
        let input = open_in fname in
        let input2 = open_in "BuildinFunctions.sti" in
        let lexbuf = Lexing.from_channel input in
        let lexbuf2= Lexing.from_channel input2 in
        let program = Parser.program Scanner.token lexbuf in
        let program2 = Parser.program Scanner.token lexbuf2 in
        let program_t = Typecheck.check_program (fst program, (snd program @ snd program2)) in        
        let lindex = ( if ( String.contains fname '/') then ((String.rindex fname '/') + 1) else 0 ) in
        let temp = String.capitalize (String.sub fname lindex ( index - lindex) ) in 
        let outfilename = temp ^".java" in
        if action = Ast then let listing = Ast.string_of_program program in print_string listing
        else
          if action = Sast then let listing = Sast.string_of_program_t program_t in print_string listing
          else
            let listing = Translator.to_java program_t temp in           
            let out = open_out outfilename in
                output_string out listing; close_out out;
            if action = Class then execvp "javac" [|"javac"; outfilename|]
          
