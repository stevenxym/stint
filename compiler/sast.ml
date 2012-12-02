type bop_t = Add | Sub | Mult | Div | Equal | Neq | Less | LessEq | Grt | GrtEq | And | Or
type uop_t = Not
type sop_t = Adds | Subs | Eqs | Neqs
type subs_t = SubChar | SubInt | SubStr
type sets_t = Spl | Fnd
type strm_t = In | Out

type expr_t = 
	  Integer of int
	| String of string
	| Id of string
	| BinOp of expr * bop * expr	(* general operator for int & bollean *)
	| UniOp of uop * expr		(* special for ! *)
	| StrOp of expr * sop * expr	(* operator for string *)
	| StrOpAt of expr * sop * expr * expr	(* with @ *)
	| Assign of string * expr
	| AssignStr of string * expr	(* assign value to string type *)
	| AssignSet of string * subs * expr * expr
	| Extract of string * subs * expr
	| Sublen of string * expr * expr
	| Chset of string * sets * expr
	| RemoveSet of string * subs * expr
	| RemoveStr of string * expr * expr
	| Stream of strm * string * expr
	| Call of string * expr list
	| IntToStr of expr		(* type convert *)
	| BoolToStr of expr		(* type convert *)

type fop_t = Open | Close

type stmt_t = 
	  Block of stmt list
	| Decl of (string * string * expr)
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt
	| While of expr * stmt
	| Break
	| Fop of fop * expr

type func_t = {
	returnType :string;
	fname : string;
	formals : (string * string * expr) list;
	body : stmt list; }

type prog_t = (string * string * expr) list * func_decl list
