open Sast

let depth = ref 0

let rec tabs_helper = function
	0 -> ""
	| x -> "  " ^ tabs_helper (x-1)
	
let rec tabs = function
	0 -> tabs_helper depth.contents
	| x -> ""

let rec string_of_expr = function
    Integer(i) -> string_of_int i
  | String(s) -> "new Stint(" ^ s ^ ")"
  | Boolean(b) -> 
  		(match b with 
  			True -> "true"
  			| False -> "false"  )
  | Id(s) -> s
  | BinOp(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
		    Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      	| Equal -> "==" | Neq -> "!=" | Less -> "<" | LessEq -> "<=" 
      	| Grt -> ">" | GrtEq -> ">=" | And -> "&&" | Or -> "||" ) 
      ^ " " ^ string_of_expr e2
  | UniOp(o, e1) ->
 	  (match  o with 
 	  	Not -> "!" ) ^ " " ^ string_of_expr e1
  | StrOp(e1, o, e2) ->  
  		string_of_expr e1 ^ 
  		(match o with
			 Adds -> ".add(" ^ string_of_expr e2 ^ ")"
  			| Subs -> ".minus(" ^ string_of_expr e2 ^ ")"
  			| Eqs -> ".equals(" ^ string_of_expr e2 ^ ")"
  			| Neqs -> ".nonEquals(" ^ string_of_expr e2 ^ ")")
  | StrOpAt(e1, o, e2, e3) ->
  		string_of_expr e1 ^
  		(match o with
  			Adds -> ".addAt(" ^ string_of_expr e2 ^", "^ string_of_expr e3 ^ ")"
  			| Subs-> ".minusAt(" ^ string_of_expr e2 ^", "^ string_of_expr e3 ^ ")"
  			| Neqs -> ""
  			| Eqs -> "") 
  | Assign(s1, e2) ->
  		s1 ^ " = " ^ string_of_expr e2
  | AssignStr(s1, e2) ->
  		s1 ^ " = new Stint (" ^ string_of_expr e2 ^ ")"
  | AssignSet(s1, st, e1, e2) ->
  		s1 ^ (match st with
  			SubChar -> ".setByIndex(" ^ string_of_expr e2 ^", "^ string_of_expr e1 ^")"
  			| SubInt -> ".setByInt(" ^ string_of_expr e2 ^", "^ string_of_expr e1 ^ ")"
  			| SubStr -> ".setByString(" ^ string_of_expr e2 ^", "^ string_of_expr e1 ^ ")")
  | AssignRange(s1, e1, e2, e3) ->
      s1 ^ ".setByRange(" ^ string_of_expr e3 ^", "^ string_of_expr e1 ^", "^string_of_expr e2 ^")" 
  | Extract(s, st, e) ->
  		s ^
  		(match st with
  			SubChar -> ".getSubstring(" ^ string_of_expr e ^")"
  			| SubInt -> ".getInt(" ^ string_of_expr e ^ ")"
  			| SubStr -> ".getString(" ^ string_of_expr e ^ ")"   )
  | Sublen(s, e1, e2) ->
  		s ^ ".getSubstring(" ^ string_of_expr e1 ^
  			", " ^ string_of_expr e2 ^ ")"
  | Chset(s,set, e) -> 
  		s ^ 
  		(match set with 
  			Spl -> ".split(" ^ string_of_expr e ^ ")"
  			| Fnd -> ".getCount(" ^ string_of_expr e^")")
  | RemoveSet(s, st, e) -> 
  		s ^ 
  		(match st with
  			SubChar -> ".removeChar(" ^ string_of_expr e ^ ")"
  			| SubInt -> ".removeInt(" ^ string_of_expr e ^ ")"
  			| SubStr -> ".removeString(" ^ string_of_expr e ^")")
  | RemoveRange(s, e1, e2) ->
  		s ^ 
  		".removeRange(" ^ string_of_expr e1 ^", " ^ string_of_expr e2 ^")" 	
  | Stream(s, e1, e2) ->  
     (* (let str = string_of_expr e1 in
          let temp = String.sub str 10 ((String.rindex str '.') - 10) in  *)
          (match s with
            In -> (* "Stint "^ string_of_expr e ^"=null;\n "*)
                "in = Utility.getScanner(" ^ string_of_expr e1 ^ "); \n
                while (in.hasNextLine()) {\n  "^ string_of_expr e2 ^" = new Stint(in.nextLine());\n System.out.println("^ string_of_expr e2 ^".toString());\n }"
            | Out -> "pwriter = Utility.getWriter(" ^string_of_expr e1 ^ ");\n pwriter.print((" ^ string_of_expr e2 ^").toString())"
          )
      
  | StreamStd(s, e) ->
      (match s with
        In -> "\tScanner in = new Scanner(System.in);\n\t" ^ string_of_expr e ^ " = new Stint( in.next() )"
        | Out -> "\tSystem.out.print((" ^ string_of_expr e ^ ").toString())"
      )  
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ToStr(e) -> "new Stint(" ^ string_of_expr e ^ ")"
  | Noexpr -> ""
  | Fop(fop, e) -> (* let str = string_of_expr e in
          let temp = String.sub str 10 ((String.rindex str '.') - 10) in *)
        (match fop with 
  			 Open -> "try { \n\tUtility.getFile(" ^ string_of_expr e ^");\n\tScanner in;\n\tPrintWriter pwriter;\n" (* = new PrintWriter(new FileWriter(Utility.getFile(" ^ string_of_expr e ^ "))) *)
  			 | Close -> "if (Utility.close(" ^ string_of_expr e ^ ")) \n\tSystem.out.print(\"Close file successfully.\");\n\t} \n\tcatch (Exception e) { \n System.err.println (e); }\n" ) 

let string_of_var = function
    (s1, s2, e) -> tabs 0 ^ (if s1 = "int" || s1 = "boolean" then s1 else "Stint" )^ " " ^ s2 ^ (if e = Noexpr then ";" else " = "^ string_of_expr e ^ ";\n")
       

let rec string_of_stmt = function
    Block(stmts) -> string_of_block stmts
  | Decl(str1, str2, expr) -> tabs 0 ^ string_of_var (str1, str2, expr)
  | Expr(expr) -> tabs 0 ^ string_of_expr expr ^ ";\n"
  | Return(expr) -> tabs 0 ^ "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, Block([])) -> tabs 0 ^ "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  tabs 0 ^ "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> tabs 0 ^ "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> tabs 0 ^ "break;\n"

and string_of_block (sl) =
	let s = "\n" ^ tabs 0 ^ "{\n" in
	depth := depth.contents + 1;
	let s = s ^ String.concat "" (List.map string_of_stmt sl) in
	depth := depth.contents - 1; s ^ tabs 0 ^ "}\n\n"

let string_of_formal = function
  (s1, s2, e) -> (if s1 = "int" || s1 = "boolean" then s1 ^ " " ^ s2 
      else "Stint" ^" "^ s2 )

let string_of_formal_list = function
[] -> ""
| formals -> String.concat ", " (List.map string_of_formal formals)

let string_of_fdecl fdecl = tabs 0 ^
  (if fdecl.fname = "main" then "public static void main (String args[]) \n"
    else (if fdecl.returnType = "int" || fdecl.returnType = "boolean" then fdecl.returnType else "Stint" )
          ^ " " ^ fdecl.fname ^ "(" ^ string_of_formal_list fdecl.formals ^ ")\n" 
    )
  	^ string_of_block  fdecl.body

let java_func_list = function
	[] -> ""
	|funcs -> String.concat "\n" (List.map string_of_fdecl funcs)

let java_var_list = function
	[] -> ""
	| vars -> (String.concat "" (List.map string_of_var vars))

let to_java (vars_t, funcs_t) name =
 	depth := 0; 
	"import java.util.Scanner;\nimport java.io.*; \nimport java.io.IOException; \n\nclass "^ name ^ "{ \n" 
	^ java_var_list vars_t ^ "\n" ^ java_func_list funcs_t ^ "\n }" 
  
