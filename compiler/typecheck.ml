open Ast
open Symboltable

module StringMap = Map.Make(String)

(* get the type of expression:
 *  -> string if one of the two operands having string type
 *  -> int/boolean if both of the operands having the same type *)
let get_expr_type t1 t2 =
	if t1 == "void" || t2 == "void" then raise (Failure ("cannot use void type inside expression")) else
	if t1 == "string" || t2 == "string" then "string" else
	if t1 == "int" && t2 == "int" then "int" else
	if t1 == "boolean" && t2 == "boolean" then "boolean" else
	raise (Failure ("type error"))

(* mark int & boolean expression to string type *)
let conv_type = function 
	(expr, t) -> if t == "void" then raise (Failure ("cannot use void type inside expression")) else
		     if t != "string" then Sast.ToStr(expr) else expr

(* get variable type according to the name
 * raise error if no name matching in variable list *)
let get_vartype env id = 
	let t = find_variable id env in
	if t == "" then raise (Failure ("undefined variable " ^ id)) else t

let check_string_id expr = 
	let (e, t) = expr in
	if t != "string" then raise (Failure ("type error")) else
	( match e with Sast.Id(i) -> e
			| _ -> raise (Failure ("should use identifier")) )

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
	  Add -> (Sast.StrOpAt((fst e1), Sast.Adds, (fst e2), pos), "string")
	  | Sub -> (Sast.StrOpAt((fst e1), Sast.Subs, (fst e2), pos), "string")
	  | _ -> raise (Failure ("type error"))

let rec check_expr env = function
	Integer(i) -> Sast.Integer(i), "int"
	| String(s) -> Sast.String(s), "string"
	| Boolean(b) -> (match b with True -> (Sast.Boolean(Sast.True), "boolean")
			| False -> (Sast.Boolean(Sast.False), "boolean"))

	| Id(id) ->
		Sast.Id(id), (get_vartype env id)

	| Oper(e1, op, e2) ->
		match_oper (check_expr env e1) op (check_expr env e2)

	| Not(e) ->
		Sast.UniOp(Sast.Not, (get_expr_with_type env e "boolean")), "boolean"

	| OperAt(e1, op, e2, pos) ->
		let fst_expr = check_expr env e1
		and snd_expr = check_expr env e2
		and position = get_expr_with_type env pos "int" in
		if (get_expr_type (snd fst_expr) (snd snd_expr)) != "string" then raise (Failure ("type error"))
		else match_str_oper fst_expr op snd_expr position

	| Assign(id, e) ->
		let t = get_vartype env id in
		if t == "string" then 
		     Sast.AssignStr(id, (conv_type (check_expr env e))), "void"
		else Sast.Assign(id, (get_expr_with_type env e t)), "void"

	| AssignSet(id, subs, i, e) ->
		if (get_vartype env id) != "string" then raise (Failure ("type error"))
		else let index = get_expr_with_type env i "int"
		     and expr = check_expr env e in
		     ( match subs with
			  SubChar -> Sast.AssignSet(id, Sast.SubChar, index, (conv_type expr)), "void"
			| SubInt  -> if (snd expr) != "int" then raise (Failure ("type error"))
				     else Sast.AssignSet(id, Sast.SubInt, index, (fst expr)), "void"
			| SubStr  -> Sast.AssignSet(id, Sast.SubStr, index, (conv_type expr)), "void" )

	| AssignRange(id, i, l, e) ->
		if (get_vartype env id) != "string" then raise (Failure ("type error"))
		else Sast.AssignRange(  id,
					get_expr_with_type env i "int",
					get_expr_with_type env l "int",
					conv_type (check_expr env e)), "void"

	| Extract(id, subs, i) ->
		if (get_vartype env id) != "string" then raise (Failure ("type error"))
		else let index = get_expr_with_type env i "int" in
		     ( match subs with
			  SubChar -> Sast.Extract(id, Sast.SubChar, index), "string"
			| SubInt  -> Sast.Extract(id, Sast.SubInt, index), "int"
			| SubStr  -> Sast.Extract(id, Sast.SubStr, index), "string" )

	| Sublen(id, i, l) ->
		if (get_vartype env id) != "string" then raise (Failure ("type error"))
		else Sast.Sublen( id,
				  get_expr_with_type env i "int",
				  get_expr_with_type env l "int"), "string"

	| Chset(id, sets, s) ->
		if (get_vartype env id) != "string" then raise (Failure ("type error"))
		else let str = get_expr_with_type env s "string" in
		     ( match sets with
			  Spl -> Sast.Chset(id, Sast.Spl, str), "int"
			| Fnd -> Sast.Chset(id, Sast.Fnd, str), "int" )
			
	| RemoveSet(id, subs, i) ->
		if (get_vartype env id) != "string" then raise (Failure ("type error"))
		else let index = get_expr_with_type env i "int" in
		     ( match subs with
			  SubChar -> Sast.RemoveSet(id, Sast.SubChar, index), "void"
			| SubInt -> Sast.RemoveSet(id, Sast.SubInt, index), "void"
			| SubStr -> Sast.RemoveSet(id, Sast.SubStr, index), "void" )

	| RemoveRange(id, i, l) ->
		if (get_vartype env id) != "string" then raise (Failure ("type error"))
		else Sast.RemoveRange(  id,
					get_expr_with_type env i "int",
					get_expr_with_type env l "int"), "void"
	| Stream(strm, dest, e) ->
		let target = get_expr_with_type env dest "string"
		and expr = check_expr env e in
		( match strm with
			In -> Sast.Stream(Sast.In, target, check_string_id expr), "void"
			| Out -> Sast.Stream(Sast.Out, target, conv_type expr), "void" )

	| StreamStd(strm, e) ->
		let expr = check_expr env e in
		( match strm with
			In -> Sast.StreamStd(Sast.In, check_string_id expr), "void"
			| Out -> Sast.StreamStd(Sast.Out, conv_type expr), "void" )

	| Call(func, e_list) -> Sast.Call(func, e_list)
	| Fop(fop, e) ->
		let target = get_expr_with_type env e "string" in
		( match fop with
			Open -> Sast.Fop(Sast.Open, target), "void"
			| Close -> Sast.Fop(Sast.Close, target), "void" )

	| Noexpr -> Sast.Noexpr, "void"

(* get expr_t(sast type) by expr(ast type) with given type
 * raise error if the expression type does match requirement *)
and get_expr_with_type env expr t = 
	let e = check_expr env expr in
	if (snd e) != t then raise (Failure ("type error")) else (fst e)

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

