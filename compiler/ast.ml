type op = 	Add | Sub | Mult | Div						(* +, -, *, / *)
					| Equal | Neq												(* ==, != *)
					| Less | LessEq | Grt | GrtEq				(* <, <=, >, >= *)
					| And | Or | Not										(* &&, ||, ! *)
type subs = SubChar | SubInt | SubStr					(* [] .<>, <> *)
type sets = Spl | Fnd													(* |, # *)
type strm = In | Out													(* <<, >>*)

type expr =
	  Integer of int													(* data type: int *)
	| String of string												(* data type: string *)
	| Id of string														(* indentifier *)
	| Oper of expr * op * expr
	| OperAt of expr * op * expr * expr				(* expr1 + expr2 @ pos *)
	| Assign of string * expr
	| Extract of string * subs *int
	| Sublen of string * int * int						(* str[index, length] *)
	| Chset of string * sets *string					(* change set *)
	| Remove of string
	| Stream of strm * expr * expr						(* io stream *)
	| Call of string * expr list
	| Noexpr																	(* for void arg list *)

type fop = Open | Close											(* file operator *)

type stmt =
	| Block of stmt list
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt								(* if() {} else{} *)
	| While of expr * stmt										(* while() {} *)
	| Break
	| Fop of fop * expr												(* open/close filename *)

type func_decl = {
		fname : string;													(* function name *)
		args : string list;											(* arguments passed in *)
		local : string list;										(* variables defined in function *)
		body : stmt list;												(* function statements *)
	}

