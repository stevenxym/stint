type bop_t = Add | Sub | Mult | Div | Equal | Neq | Less | LessEq | Grt | GrtEq | And | Or
type uop_t = Not
type sop_t = Adds | Subs | Eqs | Neqs
type subs_t = SubChar | SubInt | SubStr
type sets_t = Spl | Fnd
type strm_t = In | Out 
type fop_t = Open | Close
type boolean_t = True | False

type expr_t = 
	  Integer of int
	| String of string
	| Boolean of boolean_t
	| Id of string
	| BinOp of expr_t * bop_t * expr_t	(* general operator for int & bollean *)
	| UniOp of uop_t * expr_t		(* special for ! *)
	| StrOp of expr_t * sop_t * expr_t	(* operator for string *)
	| StrOpAt of expr_t * sop_t * expr_t * expr_t	(* with @ *)
	| Assign of string * expr_t
	| AssignStr of string * expr_t	(* assign value to string type *)
	| AssignSet of string * subs_t * expr_t * expr_t     
	| AssignRange of string * expr_t * expr_t* expr_t
	| Extract of string * subs_t * expr_t
	| Sublen of string * expr_t * expr_t
	| Chset of string * sets_t * expr_t
	| RemoveSet of string * subs_t * expr_t
	| RemoveRange of string * expr_t * expr_t
	| Stream of strm_t * expr_t * expr_t
	| StreamStd of strm_t * expr_t
	| Call of string * expr_t list
	| ToStr of expr_t		(* type convert *)
	| Fop of fop_t * expr_t
	| Noexpr

type stmt_t = 
	  Block of stmt_t list
	| Decl of (string * string * expr_t)
	| Expr of expr_t
	| Return of expr_t
	| If of expr_t * stmt_t * stmt_t
	| While of expr_t * stmt_t
	| Break


type func_t = {
	returnType :string;
	fname : string;
	formals : (string * string * expr_t) list;
	body : stmt_t list; }

type prog_t = (string * string * expr_t) list * func_t list


let rec string_of_expr_t = function
    Integer(i) -> string_of_int i
  | String(s) -> s
  | Id(s) -> s
  | Boolean(b) -> (match b with True -> "true" | False -> "false")
  | BinOp(e1, o, e2) ->
      string_of_expr_t e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | LessEq -> "<=" | Grt -> ">" | GrtEq -> ">=" | And -> "&&" | Or -> "||") ^ " " ^
      string_of_expr_t e2
  | UniOp(o, e) -> "!" ^ string_of_expr_t e
  | StrOp(e1, o, e2) ->
  		string_of_expr_t e1 ^ " " ^
      	(match o with
		Adds -> "+" | Subs -> "-" | Eqs -> "==" | Neqs -> "!=" ) ^ " " ^
     	string_of_expr_t e2
  | StrOpAt(e1, bop, e2, e3) ->
  		string_of_expr_t e1 ^ " " ^
  		(match bop with
  		Adds -> "+" | Subs -> "-" | Eqs -> "==" | Neqs -> "!=" ) ^ " " ^
      	string_of_expr_t e2 ^ " @ " ^ string_of_expr_t e3
  | Assign(v, e) -> v ^ " = " ^ string_of_expr_t e
  | AssignStr(v, e) -> v ^ " = " ^ string_of_expr_t e
  | AssignSet(v, s, e1, e2) -> v ^ 
  	(match s with 
  		SubChar -> "[" ^ string_of_expr_t e1 ^ "]"
  		| SubInt -> ".<|" ^ string_of_expr_t e1 ^ "|>"
  		| SubStr -> "<|" ^ string_of_expr_t e1 ^ "|>")
  		^ " = " ^ string_of_expr_t e2
  | AssignRange(v, i, l, e) -> v ^"["^ string_of_expr_t i ^","^ string_of_expr_t l^"]=" ^ string_of_expr_t e
  | Extract(v, s, e) -> v ^ (match s with 
  				SubChar -> "[" ^ string_of_expr_t e ^ "]"
  				| SubInt -> ".<|" ^ string_of_expr_t e ^ "|>"
  				| SubStr -> "<|" ^ string_of_expr_t e ^ "|>")
  | Sublen(v, e1, e2) -> v ^ "[" ^ string_of_expr_t e1  ^ ", " ^ string_of_expr_t e2 ^ "]"
  | Chset(v, s, e) -> v ^ (match s with
  				Spl -> " | "
  				| Fnd -> " # ") ^ string_of_expr_t e
  | RemoveSet(v, s, e) -> "~" ^ v ^ (match s with 
  				SubChar -> "[" ^ string_of_expr_t e ^ "]"
  				| SubInt -> ".<|" ^ string_of_expr_t e ^ "|>"
  				| SubStr -> "<|" ^ string_of_expr_t e ^ "|>")
  | RemoveRange(v, e1, e2) -> "~" ^ v ^ "[" ^ string_of_expr_t e1 ^ ", " ^ string_of_expr_t e2 ^ "]"
  | Stream(s, v, e) ->  string_of_expr_t v ^ (match s with
  				In -> " >> "
  				| Out -> " << ") ^ string_of_expr_t e
  | StreamStd(s, e) -> "std" ^ (match s with
  				In -> " >> " | Out -> " << ") ^ string_of_expr_t e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr_t el) ^ ")"
  | ToStr(e) -> string_of_expr_t e
  | Noexpr -> ""
  | Fop(fop, e) -> (match fop with Open -> "Open " | Close -> "Close ") ^ string_of_expr_t e

let rec string_of_stmt_t = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt_t stmts) ^ "}\n"
  | Decl(str1, str2, expr) -> str1 ^ " " ^ str2 ^ " = " ^ string_of_expr_t expr ^ ";\n"
  | Expr(expr) -> string_of_expr_t expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr_t expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr_t e ^ ")\n" ^ string_of_stmt_t s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr_t e ^ ")\n" ^
      string_of_stmt_t s1 ^ "else\n" ^ string_of_stmt_t s2
  | While(e, s) -> "while (" ^ string_of_expr_t e ^ ") " ^ string_of_stmt_t s
  | Break -> "break;\n"


let string_of_fdecl_t fdecl =
  fdecl.returnType ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map (fun (t, n, _) -> t ^ n) fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt_t fdecl.body) ^
  "}\n"

let string_of_program_t (vars, funcs) =
  String.concat ";\n" (List.map (fun (t, n, e) -> t ^ " " ^ n ^ " " ^ string_of_expr_t e) vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl_t funcs)

