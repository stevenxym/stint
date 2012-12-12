open Ast
open Symboltable

module StringMap = Map.Make(String)

let get_expr_type t1 t2 =
	if t1 == "string" || t2 == "string" then "string" else
	if t1 == "int" && t2 == "int" then "int" else
	if t1 == "boolean" && t2 == "boolean" then "boolean" else
	raise (Failure ("type error"))

let conv_type = function 
	(expr, t) -> if t != "string" then Sast.ToStr(expr) else expr

let match_oper e1 op e2 =
	let expr_t = get_expr_type (snd e1) (snd e2) in
	(match op with
	   Add -> if expr_t == "int" then (Sast.BinOp(fst e1, Sast.Add, fst e2), "int") else
	   	  if expr_t == "string" then (Sast.StrOp(conv_type e1, Sast.Adds, conv_type e2), "string") else
		  raise (Failure ("type error"))
	 | Sub -> if expr_t == "int" then (Sast.BinOp(fst e1, Sast.Sub, fst e2), "int") else
	 	  if expr_t == "string" then (Sast.StrOp(conv_type e1, Sast.Subs, conv_type e2), "string") else
		  raise (Failure ("type error"))
	 | Mult -> if expr_t == "int" then (Sast.BinOp(fst e1, Sast.Mult, fst e2), "int") else
	 	   raise (Failure ("type error"))
	 | Div -> if expr_t == "int" then (Sast.BinOp(fst e1, Sast.Div, fst e2), "int") else
		  raise (Failure ("type error"))
	 | Equal -> if expr_t == "string" then (Sast.StrOp(conv_type e1, Sast.Eqs, conv_type e2), "boolean") else
	 	    (Sast.BinOp(fst e1, Sast.Equal, fst e2), "boolean")
	 | Neq -> if expr_t == "string" then (Sast.StrOp(conv_type e1, Sast.Neqs, conv_type e2), "boolean") else
	 	  (Sast.BinOp(fst e1, Sast.Neq, fst e2), "boolean")
	 | Less -> if expr_t == "string" then raise (Failure ("type error")) else
	 	   (Sast.BinOp(fst e1, Sast.Less, fst e2), "boolean")
	 | LessEq -> if expr_t == "string" then raise (Failure ("type error")) else
	 	     (Sast.BinOp(fst e1, Sast.LessEq, fst e2), "boolean")
	 | Grt -> if expr_t == "string" then raise (Failure ("type error")) else
	 	  (Sast.BinOp(fst e1, Sast.Grt, fst e2), "boolean")
	 | GrtEq -> if expr_t == "string" then raise (Failure ("type error")) else
	 	    (Sast.BinOp(fst e1, Sast.GrtEq, fst e2), "boolean")
	 | And -> if expr_t == "string" then raise (Failure ("type error")) else
	 	  (Sast.BinOp(fst e1, Sast.And, fst e2), "boolean")
	 | Or -> if expr_t == "string" then raise (Failure ("type error")) else
	 	 (Sast.BinOp(fst e1, Sast.Or, fst e2), "boolean")
	)

let match_str_oper e1 op e2 pos =
	match op with
	  Add -> (Sast.StrOpAt((fst e1), Sast.Adds, (fst e2), (fst pos)), "string")
	  | Sub -> (Sast.StrOpAt((fst e1), Sast.Subs, (fst e2), (fst pos)), "string")
	  | _ -> raise (Failure ("type error"))

let rec check_expr env = function
	Integer(i) -> (Sast.Integer(i), "int")
	| String(s) -> (Sast.String(s), "string")
	| Boolean(b) -> (match b with True -> (Sast.Boolean(Sast.True), "boolean")
			| False -> (Sast.Boolean(Sast.False), "boolean"))
	| Id(id) ->
		let t = find_variable id env in
		if t == "" then raise (Failure ("undefined variable " ^ id))
		else (Sast.Id(id), t)
	| Oper(e1, op, e2) ->
		let fst_expr = check_expr env e1 in
		let snd_expr = check_expr env e2 in
		match_oper fst_expr op snd_expr
	| Not(e) ->
		let (expr, t) = check_expr env e in
		if t == "boolean" then (Sast.UniOp(Sast.Not, expr), "boolean") else
		raise (Failure ("type error"))
	| OperAt(e1, op, e2, pos) ->
		let fst_expr = check_expr env e1 in
		let snd_expr = check_expr env e2 in
		let position = check_expr env pos in
		if (snd position) != "int" then raise (Failure ("type error")) else
		if (get_expr_type (snd fst_expr) (snd snd_expr)) != "string" then raise (Failure ("type error"))
		else match_str_oper fst_expr op snd_expr position
	| Assign(id, e) ->
		let t = find_variable id env in
		let expr = check_expr env e in
		if t == "" then raise (Failure ("type error")) else
		if t != (snd expr) then raise (Failure ("type error")) else
		if t == "string" then (Sast.AssignStr(id, (fst expr)), t) else
		   (Sast.Assign(id, (fst expr)), t)
	| AssignSet(id, subs, i, e) -> Sast.AssignSet(id, subs, i, e)
	| AssignRange(id, index, len, e) -> Sast.AssignRange(id, index, len, e)
	| Extract(id, subs, i) -> Sast.Extract(id, subs, i)
	| Sublen(id, i, len) -> Sast.Sublen(id, i, len)
	| Chset(id, sets, str) -> Sast.Chset(id, sets, str)
	| RemoveSet(id, subs, i) -> Sast.RemoveSet(id, subs, i)
	| RemoveStr(id, i, len) -> Sast.RemoveStr(id, i, len)
	| Stream(strm, dest, e) -> Sast.Stream(strm, dest, e)
	| Call(func, e_list) -> Sast.Call(func, e_list)
	| Fop(fop, e) -> Sast.Fop(fop, e)
	| Noexpr -> Sast.Noexpr

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
	| Decl(s1, s2, expr) -> let e = check_expr env expr in Sast.Decl(s1, s2, e.fst)
	| Expr(expr) -> (check_expr env expr).fst
	| Return(expr) -> (check_expr env expr).fst
	| If(expr, stmt1, stmt2) ->	let e = check_expr env expr in 
								if e.snd != "boolean" then raise (Failure ("The type of the condition in if statement must be boolean!")) 
								else Sast.If(e.fst, (check_stmt env stmt1), (check_stmt env stmt2))	(* if() {} else{} *)
	| While(expr, stmt) -> let e = check_expr env expr in
						   if e.snd != "boolean" then raise (Failure ("The type of the condition in while statement must be boolean!"))
						   else Sast.While(e.fst, (check_stmt env stmt))				(* while() {} *)
	| Break -> Sast.Break

and check_stmt_list env = function 
	  [] -> []
	| hd::tl -> (check_stmt env hd) :: (check_stmt_list env tl)

