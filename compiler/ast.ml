type op = Add | Sub | Mult | Div						(* +, -, *, / *)
type eq = Equal | Neq												(* ==, != *)
type cmp = Less | LessEq | Grt | GrtEq			(* <, <=, >, >= *)
type subs = SubInt | SubStr									(* .<>, <> *)
type sets = Spl | Fnd												(* |, # *)
type rm = Rmv																(* ~ *)
type lg = And | Or | Not										(* &&, ||, ! *)
type strm = In | Out												(* <<, >>*)

type expr =
	  Integer of int													(* data type: int *)
	| String of str														(* data type: string *)
	| Boolean of bool													(* data type: boolean *)
	| Id of string														(* indentifier *)
	| Oper of expr * op * expr
	| OperAt of expr * op * expr * expr				(* expr1 + expr2 @ pos *)
	| Assign of string * expr
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

