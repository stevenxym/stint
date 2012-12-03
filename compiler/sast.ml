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
	| BinOp of expr_t * bop_t * expr_t	(* general operator for int & bollean *)
	| UniOp of uop_t * expr_t		(* special for ! *)
	| StrOp of expr_t * sop_t * expr_t	(* operator for string *)
	| StrOpAt of expr_t * sop_t * expr_t * expr_t	(* with @ *)
	| Assign of string * expr_t
	| AssignStr of string * expr_t	(* assign value to string type *)
	| AssignSet of string * subs_t * expr_t * expr_t
	| Extract of string * subs_t * expr_t
	| Sublen of string * expr_t * expr_t
	| Chset of string * sets_t * expr_t
	| RemoveSet of string * subs_t * expr_t
	| RemoveStr of string * expr_t * expr_t
	| Stream of strm_t * string * expr_t
	| Call of string * expr_t list
	| IntToStr of expr_t		(* type convert *)
	| BoolToStr of expr_t		(* type convert *)
	| Fop of fop_t * expr_t
	| NoExpr

type fop_t = Open | Close

type stmt_t = 
	  Block of stmt_t list
	| Decl of (string * string * expr_t)
	| expr_t of expr_t
	| Return of expr_t
	| If of expr_t * stmt_t * stmt_t
	| While of expr_t * stmt_t
	| Break

type func_t = {
	returnType :string;
	fname : string;
	formals : (string * string * expr_t) list;
	body : stmt list; }

type prog_t = {
	vars: (string * string * expr_t) list;
	funcs: func_t list;
	}
