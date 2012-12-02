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
	| AssignSet of string * subs * expr * expr
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
