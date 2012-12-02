type bop = Add | Sub | Mult | Div | Equal | Neq | Less | LessEq | Grt | GrtEq | And | Or
type uop = Not
type sop = Adds | Subs | Eqs | Neqs
type subs = SubChar | SubInt | SubStr
type sets = Spl | Fnd
type strm = In | Out

type expr = 
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

type fop = Open | Close

type stmt = 
	  Block of stmt list
	| Decl of (string * string * expr)
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt
	| While of expr * stmt
	| Break
	| Fop of fop * expr

type func_decl = {
	returnType :string;
	fname : string;
	formals : (string * string * expr) list;
	body : stmt list; }

type program = (string * string * expr) list * func_decl list
