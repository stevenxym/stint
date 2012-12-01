type op = 	Add | Sub | Mult | Div						(* +, -, *, / *)
					| Equal | Neq						(* ==, != *)
					| Less | LessEq | Grt | GrtEq		(* <, <=, >, >= *)
					| And | Or | Not					(* &&, ||, ! *)
type subs = SubChar | SubInt | SubStr					(* [] .<>, <> *)
type sets = Spl | Fnd									(* |, # *)
type strm = In | Out									(* <<, >>*)


type expr =
	  Integer of int									(* data type: int *)
	| String of string									(* data type: string *)
	| Id of string										(* indentifier *)
	| Std of string
	| Oper of expr * op * expr
	| OperAt of expr * op * expr * expr	(* expr1 + expr2 @ pos *)
	| Assign of string * expr		(* iden = *)
	| AssignSet of string * subs * expr * expr
	| Extract of string * subs *int
	| Sublen of string * int * int						(* str[index, length] *)
	| Chset of string * sets *string					(* change set *)
	| RemoveSet of string * * subs * int	(* ~str<||>, ~str.<||> *)
	| RemoveStr of string * int * int	(* ~str[,] *)
	| Stream strm * expr * expr				(* io stream *)
	| Call of string * expr list
	| Noexpr											(* for void arg list *)

type fop = Open | Close									(* file operator *)
	
type stmt =
	| Block of stmt list
	| Decl of string * string * expr
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt							(* if() {} else{} *)
	| While of expr * stmt								(* while() {} *)
	| Break
	| Fop of fop * expr									(* open/close filename *)

type func_decl = {
		returnType :string;
		fname : string;									(* function name *)
		formals : string list;							(* Formal argument names *)
		(* local : string list								variables defined in function *)
		body : stmt list;								(* function statements *)
	}

type program = string list * func_decl list (* global vars, funcs *)
