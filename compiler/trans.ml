open Ast
open Sast

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
  	
 





      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!=" | Less -> "<" | LessEq -> "<=" 
      | Grt -> ">" | GrtEq -> ">=" | And -> "&&" | Or -> "||") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | AssignSet(v, s, e1, e2) -> v ^ 
  								(match s with 
  									SubChar -> "[" ^ string_of_expr e1 ^ "]"
  									| SubInt -> ".<|" ^ string_of_expr e1 ^ "|>"
  									| SubStr -> "<|" ^ string_of_expr e1 ^ "|>")
  								^ " = " ^ string_of_expr e2
  | Extract(v, s, e) -> v ^ (match s with 
  									SubChar -> "[" ^ string_of_expr e ^ "]"
  									| SubInt -> ".<|" ^ string_of_expr e ^ "|>"
  									| SubStr -> "<|" ^ string_of_expr e ^ "|>")
  | Sublen(v, e1, e2) -> v ^ "[" ^ string_of_expr e1  ^ ", " ^ string_of_expr e2 ^ "]"
  | Chset(v, s, e) -> v ^ (match s with
  								Spl -> " | "
  								| Fnd -> " # ") ^ string_of_expr e
  | RemoveSet(v, s, e) -> "~" ^ v ^ (match s with 
  									SubChar -> "[" ^ string_of_expr e ^ "]"
  									| SubInt -> ".<|" ^ string_of_expr e ^ "|>"
  									| SubStr -> "<|" ^ string_of_expr e ^ "|>")
  | RemoveStr(v, e1, e2) -> "~" ^ v ^ "[" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ "]"
  | Stream(s, v, e) ->  v ^ (match s with
  						  In -> " >> "
  						  | Out -> " << ") ^ string_of_expr e 
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | Not(e) -> "!" ^ string_of_expr e
  | OperAt(e1, bop, e2, e3) -> string_of_expr e1 ^ " " ^ (match bop with
  Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | LessEq -> "<=" | Grt -> ">" | GrtEq -> ">=" | And -> "&&" | Or -> "||") ^ " " ^ string_of_expr e2 ^ " @ " ^ string_of_expr e3

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Decl(str1, str2, expr) -> str1 ^ " " ^ str2 ^ " = " ^ string_of_expr expr ^ ";\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break;\n"
  | Fop(fop, e) -> (match fop with Open -> "Open " | Close -> "Close ") ^ string_of_expr e



let java_var_type = function
	Simple(Str) -> "string"
	| Simple(Num) -> "int"
	| Simple(None) -> "void"
	| Map(k,v) ->
		"map <" ^ (match (k,v) with
			(Str,Str) -> "string,string"
			| (Str,Num) -> "string,int"
			| (Num,Num) -> "int,int"
			| (Num,Str) -> "int,string"
			| _ -> raise(Failure("internal error"))) ^ ">"



let rec to_java (vars_t, funcs_t) =
	depth := 0; 
	"Import some package \n\n" ^ java_vars vars_t ^ java_funcs funcs_t