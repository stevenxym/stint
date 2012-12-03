Open Sast

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
	Not -> "!" | _ -> raise(Failure("internal error"))

let java_sop = function
	Adds -> "add" | Subs -> "minus" | Eqs -> "euqals" | Neqs -> "nonEquals"

let rec string_of_expr = function
    Integer(i) -> string_of_int i
  | String(s) -> s
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
  | StrOp (e1, o, e2) ->  
  		string_of_expr e1 ^ 
  		(match o with
			Adds -> ".add(" ^ string_of_expr e2 ^ ")"
  			| Subs -> ".minus(" ^ string_of_expr e2 ^ ")"
  			| Eqs -> ".equals(" ^ string_of_expr e2 ^ ")"
  			| Neq -> ".nonEquals(" ^ string_of_expr e2 ^ ")")
  | StrOpAt (e1, o, e2, e3) ->
  		string_of_expr e1 ^
  		(match o with
  			Adds -> ".addAt(" ^ string_of_expr e2 ^ string_of_expr e3 ^ ")"
  			| Eqs-> ".minusAt(" ^ string_of_expr e2 ^ string_of_expr e3 ^ ")")
  | Assign (s1, e2) ->
  		string_of_expr s1 ^ " = " ^ string_of_expr e2
  | AssignStr (s1, e2) ->
  		string_of_expr s1 ^ " = " ^ string_of_expr e2
  | AssignSet (s1, st, e1, e2) ->
  		string_of_expr s1 ^ 
  		(match st with
  			SubChar -> ".getSubstring(" ^ string_of_expr e1 ^")"
  			| SubInt -> ".getInt(" ^ string_of_expr e1 ^ ")"
  			| SubStr -> ".getString(" ^ string_of_expr e1 ^ ")")
  		^ " = " ^ string_of_expr e2
  | Extract (s, st, e) ->
  		string_of_expr s ^
  		(match st with
  			SubChar -> ".getSubstring(" ^ string_of_expr e ^")"
  			| SubInt -> ".getInt(" ^ string_of_expr e ^ ")"
  			| SubStr -> ".getString(" ^ string_of_expr e ^ ")")
  | Sublen (s, e1, e2) ->
  		string_of_expr s1 ^ ".getSubstring(" ^ string_of_expr e1 ^
  			" ," ^ string_of_expr e2 ^ ")"
  | Chset (s,set, e) -> 
  		string_of_expr s1 ^ 
  		(match set with 
  			Spl -> ".split(" ^ string_of_expr e
  			| Fnd -> ".getCount(" ^ string_of_expr e)
  | RemoveSet (s, st, e) -> 
  		string_of_expr s ^ 
  		(match st with
  			SubChar -> ".removeInt(" ^ string_of_expr e ^ ")"
  			| SubInt -> ".removeInt(" ^ string_of_expr e ^ ")"
  			| SubStr -> ".removeInt(" ^ string_of_expr e ^")")
  | RemoveStr (s, e1, e2) ->
  		string_of_expr s ^ 
  		".remove(" ^ string_of_expr e1 ^"," ^ string_of_expr e2
  	
  | Stream(s, v, e) ->  v ^ (match s with
  						  In -> " >> "
  						  | Out -> " << ") ^ string_of_expr e 
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | Fop(fop, e) -> let s = f_counter.contents in 
  			f_counter := f_counter.contents + 1;
  			(match fop with 
  			Open -> 
  			"try { \n File file_" ^ string_of_int s ^ " = new File(" ^ string_of_expr e ^");\n" 
  			^ "Scanner inFile" ^ string_of_int s ^ "= new Scanner ( file_" ^ string_of_int s ^ ");\n"
  			| Close -> "inFile" ^ string_of_int s ^ ".close();\n } \n catch (Exception e) { return false; }" ) 
  			^ string_of_expr e
  | IntToStr (e) -> string_of_expr e
  | BoolToStr (e) -> string_of_expr e 
  | Not(e) -> "!" ^ string_of_expr e
 
let string_of_block (sl:stmt_t list) =
	let s = "\n" ^ tabs 0 ^ "{\n" in
	depth := depth.contents + 1;
	let s = s ^ String.concat "" (List.map string_of_stmt sl) in
	depth := depth.contents - 1; s ^ tabs 0 ^ "}\n\n"

let rec string_of_stmt = function
    Block(stmts) -> string_of_block stmts
  | Decl(str1, str2, expr) -> tabs 0 ^ str1 ^ " " ^ str2 ^ " = " ^ string_of_expr expr ^ ";\n"
  | Expr(expr) -> tabs 0 ^ string_of_expr expr ^ ";\n";
  | Return(expr) -> tabs 0 ^ "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> tabs 0 ^ "if (" ^ string_of_expr e ^ ")\n" ^ string_of_block s
  | If(e, s1, s2) ->  tabs 0 ^ "if (" ^ string_of_expr e ^ ")\n" ^ string_of_block s1 ^ "else\n" ^ string_of_block s2
  | While(e, s) -> tabs 0 ^ "while (" ^ string_of_expr e ^ ") " ^ string_of_block s
  | Break -> tabs 0 ^ "break;\n"


let string_of_formals = function
  	(f, s, l) -> f ^ s ^ string_of_expr l 

let string_of_fdecl fdecl =
  	tabs 0 ^ fdecl.returnType ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formals fdecl.formals) ^ ")" 
  	^ string_of_block  fdecl.body

let java_func_list = function
	[] -> ""
	|funcs -> String.concat "\n" (List.map string_of_fdecl funcs)

let string_of_var = function
  	(s1, s2, e) -> tabs 0 ^ s1 ^" "^ s2 ^ " = " ^ (string_of_expr e) ^  ";\n" 

let rec java_var_list = function
	[] -> ""
	| vars -> (String.concat "" (List.map string_of_var vars))


let rec to_java (vars_t, funcs_t) =
	depth := 0; 
	"import java.util.Scanner;\n import java.io.File; \n import java.io.IOException; \n\n" 
	^ java_var_list vars_t ^ "\n" ^ java_func_list funcs_t