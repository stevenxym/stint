type bop = 	Add | Sub | Mult | Div		(* +, -, *, / *)
		| Equal | Neq			(* ==, != *)
		| Less | LessEq | Grt | GrtEq	(* <, <=, >, >= *)
		| And | Or			(* &&, ||, ! *)
type subs = SubChar | SubInt | SubStr		(* [] .<||>, <||> *)
type sets = Spl | Fnd				(* |, # *)
type strm = In | Out				(* <<, >>*)

type expr =
	  Integer of int			(* data type: int *)
	| String of string			(* data type: string *)
	| Id of string				(* indentifier *)
	| Std of string
	| Oper of expr * bop * expr
	| Not of expr
	| OperAt of expr * bop * expr * expr	(* expr1 + expr2 @ pos *)
	| Assign of string * expr		(* iden = *)
	| AssignSet of string * subs * expr * expr (*str*)
	| Extract of string * subs * expr
	| Sublen of string * expr * expr	(* str[index, length] *)
	| Chset of string * sets * expr		(* change set *)
	| RemoveSet of string * subs * expr	(* ~str<||>, ~str.<||> *)
	| RemoveStr of string * expr * expr	(* ~str[,] *)
	| Stream of strm * string * expr		(* io stream *)
	| Call of string * expr list
	| Noexpr				(* for void arg list *)

type fop = Open | Close				(* file operator *)
	
type stmt =
	| Block of stmt list
	| Decl of (string * string * expr)
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt		(* if() {} else{} *)
	| While of expr * stmt			(* while() {} *)
	| Break
	| Fop of fop * expr			(* open/close filename *)

type func_decl = {
		returnType :string;
		fname : string;			(* function name *)
		formals : (string * string * expr) list;	(* Formal argument names *)
		(* local : string list		variables defined in function *)
		body : stmt list;		(* function statements *)
	}

type program = (string * string * expr) list * func_decl list (* global vars, funcs *)

let rec string_of_expr = function
    Integer(i) -> string_of_int i
  | Id(s) -> s
  | Oper(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | LessEq -> "<=" | Grt -> ">" | GrtEq -> ">=") ^ " " ^
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
  | Sublen(v, e1, e2) -> v ^ "[" ^ string_of_expr e1 ", " ^ string_of_expr e2 ^ "]"
  | Chset(v, s, e) -> v ^ (match s with
  								Spl -> " | "
  								| Fnd -> " # ") ^ string_of_expr e
  | RemoveSet(v, s, e) -> "~" ^ v ^ (match s with 
  									SubChar -> "[" ^ string_of_expr e ^ "]"
  									| SubInt -> ".<|" ^ string_of_expr e ^ "|>"
  									| SubStr -> "<|" ^ string_of_expr e ^ "|>")
  | RemoveStr(v, e1, e2) -> "~" ^ v ^ "[" ^ string_of_expr e1 ", " ^ string_of_expr e2 ^ "]"
  | Stream(s, v, e) ->  v ^ (match s with
  						  In -> " >> "
  						  | Out -> " << ") ^ string_of_expr e 
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Decl(str, str, expr) -> ""
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
