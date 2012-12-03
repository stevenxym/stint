Open Sast

let depth = ref 0

let rec tabs_helper = function
	0 -> ""
	| x -> "  " ^ tabs_helper (x-1)
	
let rec tabs = function
	0 -> tabs_helper depth.contents
	| x -> ""


let rec string_of_expr = function
    Integer(i) -> string_of_int i
  | String(s) -> s
  | Id(s) -> s
  | Boolean(b) -> (match b with True -> "true" | False -> "false")
  | Oper(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | LessEq -> "<=" | Grt -> ">" | GrtEq -> ">=" | And -> "&&" | Or -> "||") ^ " " ^
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
  | Fop(fop, e) -> (match fop with Open -> "Open " | Close -> "Close ") ^ string_of_expr e
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
 

let string_of_formals = function
  (f, s, l) -> f ^ s ^ string_of_expr l 

let string_of_fdecl fdecl =
  fdecl.returnType ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formals fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

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
	"Import some package \n\n" ^ java_var_list vars_t ^ "\n" ^ java_func_list funcs_t