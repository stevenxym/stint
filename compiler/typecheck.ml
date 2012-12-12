open Ast
open Symboltable

module StringMap = Map.Make(String)

let check_global_var env var =
	let (v_type, name, value) = var in
	let ret = add_global name v_type env in
	if StringMap.is_empty ret then raise (Failure ("variable " ^ name ^ " is already defined")) else (v_type, name, value)

let rec check_functions env funcs = 
	match funcs with
	  [] -> []
	| hd::tl -> (check_function env hd) :: (check_functions env tl) 

let check_function env func =
	let add_function func.fname func.returnType env

let check_program (vars, funcs) = 
	let env = {	locals = StringMap.empty;
			globals = StringMap.empty;
			functions = StringMap.empty }
	in
	(List.map (check_global_var env) vars, List.map (check_function env) funcs)

let rec check_stmt env func = function
	  Block(stmt_list) -> Sast.Block(check_stmt_list env func stmt_list)
	| Decl(s1, s2, expr) -> let e = check_expr env expr in Sast.Decl(s1, s2, fst e)
	| Expr(expr) -> fst (check_expr env expr)
	| Return(expr) -> let e = check_expr env expr in
					  if (snd e != func.returnType) then raise (Failure ("The return type doesn't match!"))
					  else Sast.Return(fst e) 
	| If(expr, stmt1, stmt2) ->	let e = check_expr env expr in 
								if snd e != "boolean" then raise (Failure ("The type of the condition in If statement must be boolean!")) 
								else Sast.If(fst e, (check_stmt env func stmt1), (check_stmt func env stmt2))	(* if() {} else{} *)
	| While(expr, stmt) -> let e = check_expr env expr in
						   if snd e != "boolean" then raise (Failure ("The type of the condition in While statement must be boolean!"))
						   else Sast.While(fst e, (check_stmt env func stmt))				(* while() {} *)
	| Break -> Sast.Break

and check_stmt_list env func = function 
	  [] -> []
	| hd::tl -> (check_stmt env func hd) :: (check_stmt_list env func tl)

