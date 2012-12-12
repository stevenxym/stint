open Sast

let depth = ref 0
let f_counter = ref 0

let rec tabs_helper = function
	0 -> ""
	| x -> "  " ^ tabs_helper (x-1)
	
let rec tabs = function
	0 -> tabs_helper depth.contents
	| x -> ""

let java_bop = function
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div-> "/" 
	| Equal -> "==" | Neq -> "!=" | Less -> "<"| LessEq -> "<="
	| Grt -> ">" | GrtEq -> ">=" | And -> "&&" | Or -> "||"

let java_uop = function
	Not -> "!" 

let java_sop = function
	Adds -> "add" | Subs -> "minus" | Eqs -> "euqals" | Neqs -> "nonEquals"

let rec string_of_expr = function
    Integer(i) -> string_of_int i
  | String(s) -> "new Stint(" ^ s ^ ")"
  | Boolean(b) -> 
  		(match b with 
  			True -> "true"
  			| False -> "False")
  | Id(s) -> s
  | BinOp(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
		Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      	| Equal -> "==" | Neq -> "!=" | Less -> "<" | LessEq -> "<=" 
      	| Grt -> ">" | GrtEq -> ">=" | And -> "&&" | Or -> "||") 
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
  			Adds -> ".addAt(" ^ string_of_expr e2 ^ string_of_expr e3 ^ ")"
  			| Eqs-> ".minusAt(" ^ string_of_expr e2 ^ string_of_expr e3 ^ ")"
  			| Neqs -> ""
  			| Subs -> "") 
  | Assign(s1, e2) ->
  		s1 ^ " = " ^ string_of_expr e2
  | AssignStr(s1, e2) ->
  		s1 ^ " = " ^  string_of_expr e2
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
  			| SubStr -> ".getString(" ^ string_of_expr e ^ ")")
  | Sublen(s, e1, e2) ->
  		s ^ ".getSubstring(" ^ string_of_expr e1 ^
  			" ," ^ string_of_expr e2 ^ ")"
  | Chset(s,set, e) -> 
  		s ^ 
  		(match set with 
  			Spl -> ".split(" ^ string_of_expr e
  			| Fnd -> ".getCount(" ^ string_of_expr e^")")
  | RemoveSet(s, st, e) -> 
  		s ^ 
  		(match st with
  			SubChar -> ".removeInt(" ^ string_of_expr e ^ ")"
  			| SubInt -> ".removeInt(" ^ string_of_expr e ^ ")"
  			| SubStr -> ".removeInt(" ^ string_of_expr e ^")")
  | RemoveStr(s, e1, e2) ->
  		s ^ 
  		".remove(" ^ string_of_expr e1 ^"," ^ string_of_expr e2 ^")"
  	
  | Stream(s, v, e) ->  if v = "std" then 
      (match s with
  			In -> "Scanner in = new Scanner(System.in); \n 
                 Stint " ^ string_of_expr e ^ " = new Stint(in.next()); \n"
  			| Out -> "System.out.println((" ^ string_of_expr e ^ ").toString()); \n"
      )
    else (let temp = String.sub v 0 (String.rindex v '.')  in
          (match s with
            In -> (* "Stint "^ string_of_expr e ^"=null;\n "*)
                "while (in_"^ temp ^".hasNextLine()) {\n
                "^ string_of_expr e ^" = new Stint( in_"^ temp ^".nextLine());\n
                System.out.println("^ string_of_expr e ^".toString());\n }\n
                "
            | Out -> "pwriter_"^temp^".print((" ^ string_of_expr e ^").toString());\n"
          )
      )
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")\n"
  | ToStr(e) -> "new Stint(" ^ string_of_expr e ^ ")"
  | NoExpr -> ""
  | Fop(fop, e) -> let str = string_of_expr e in
          let temp = String.sub str 10 ((String.rindex str '.') - 10) in 
        (match fop with 
  			 Open -> "try { \n File file_" ^ temp ^ " = new File((" ^ string_of_expr e ^").toString());\n
                  Scanner in_" ^ temp ^ " = new Scanner(file_" ^ temp ^ ");\n 
                  PrintWriter pwriter_"^ temp^"= new PrintWriter(new FileWriter(file_" ^ temp ^ "));" 
  			 | Close -> "in_" ^ temp ^ ".close();\n pwriter_"^temp^".close();\n } \n 
                    catch (Exception e) { \n System.err.println (e); }\n" ) 

let rec string_of_stmt = function
    Block(stmts) -> string_of_block stmts
  | Decl(str1, str2, expr) -> tabs 0 ^ str1 ^ " " ^ str2 ^ " = " ^ string_of_expr expr ^ ";\n"
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

let string_of_formals = function
  	(f, s, l) -> f ^ s ^ string_of_expr l 

let string_of_fdecl fdecl = tabs 0 ^
  (if fdecl.fname = "main" then "public static void main (String args[]) \n"
    else (if fdecl.returnType = "int" || fdecl.returnType = "bool" then fdecl.returnType else "Stint" )^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formals fdecl.formals) ^ ")\n" 
    )
  	^ string_of_block  fdecl.body

let java_func_list = function
	[] -> ""
	|funcs -> String.concat "\n" (List.map string_of_fdecl funcs)

let string_of_var = function
  	(s1, s2, e) -> tabs 0 ^ (if s1 = "int" || s1 = "bool" then s1 ^ " " ^ s2 ^ " = " ^ string_of_expr e ^ ";\n"
      else "Stint" ^" "^ s2 ^ " = " ^ string_of_expr e ^ ";\n")

let rec java_var_list = function
	[] -> ""
	| vars -> (String.concat "" (List.map string_of_var vars))


let rec to_java (vars_t, funcs_t) =
	depth := 0; 
	"import java.util.Scanner;\n import java.io.*; \n import java.io.IOException; \n\n class StintJava { \n" 
	^ java_var_list vars_t ^ "\n" ^ java_func_list funcs_t ^ "\n }"