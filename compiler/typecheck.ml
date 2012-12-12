open Ast
open Symboltable

module StringMap = Map.Make(String)

let check_global_var env var =
	let (v_type, name, value) = var in
	let ret = add_global name v_type env in
	if StringMap.is_empty ret then raise (Failure ("variable " ^ name ^ " is already defined")) else (v_type, name, value)

let check_function env funcs = 
	funcs

let check_program (vars, funcs) = 
	let env = {	locals = StringMap.empty;
			globals = StringMap.empty;
			functions = StringMap.empty }
	in
	(List.map (check_global_var env) vars, List.map (check_function env) funcs)


let rec check_stmt env = function
	  Block(stmt_list) -> Sast.Block(check_stmt_list env stmt_list)
	| Decl(stmt_list) -> Sast.Decl(check_stmt_list env stmt_list)   
	| Expr(expr) -> (check_expr env expr).fst
	| Return(expr) -> (check_expr env expr).fst
	| If(expr, stmt1, stmt2) ->	let e = check_expr env expr in 
								if e.snd != "boolean" then raise (Failure ("The type of the condition in if statement must be boolean!")) 
								else Sast.If(e.fst, (check_stmt env stmt1), (check_stmt env stmt2)	(* if() {} else{} *)
	| While(expr, stmt) -> let e = check_expr env expr in
						   if e.snd != "boolean" then raise (Failure ("The type of the condition in while statement must be boolean!"))
						   else Sast.While(e.fst, (check_stmt env stmt))				(* while() {} *)
	| Break -> Sast.Break



let rec check_stmt_list env = function 
	  [] -> 
	| hd:tl -> (check_stmt env hd) :: (check_stmt_list env tl)
