open Ast
open Symboltable

module StringMap = Map.Make(String)

(* get the type of expression:
 *  -> string if one of the two operands having string type
 *  -> int/boolean if both of the operands having the same type *)
let get_expr_type t1 t2 =
	if t1 = "string" || t2 = "string" then "string" else
	if t1 = "int" && t2 = "int" then "int" else
	if t1 = "boolean" && t2 = "boolean" then "boolean" else
	raise (Failure ("type error"))

(* mark int & boolean expression to string type *)
let conv_type = function 
	(expr, t) -> if t != "string" then Sast.ToStr(expr) else expr

(* get variable type according to the name
 * raise error if no name matching in variable list *)
let get_vartype env id = 
	let t = find_variable id env in
	if t = "" then raise (Failure ("undefined variable " ^ id)) else t

let match_oper e1 op e2 =
	let expr_t = get_expr_type (snd e1) (snd e2) in
	(match op with
	   Add -> if expr_t = "int" then (Sast.BinOp(fst e1, Sast.Add, fst e2), "int") else
	   	  if expr_t = "string" then (Sast.StrOp(conv_type e1, Sast.Adds, conv_type e2), "string") else
		  raise (Failure ("type error"))
	 | Sub -> if expr_t = "int" then (Sast.BinOp(fst e1, Sast.Sub, fst e2), "int") else
	 	  if expr_t = "string" then (Sast.StrOp(conv_type e1, Sast.Subs, conv_type e2), "string") else
		  raise (Failure ("type error"))
	 | Mult -> if expr_t = "int" then (Sast.BinOp(fst e1, Sast.Mult, fst e2), "int") else
	 	   raise (Failure ("type error"))
	 | Div -> if expr_t = "int" then (Sast.BinOp(fst e1, Sast.Div, fst e2), "int") else
		  raise (Failure ("type error"))
	 | Equal -> if expr_t = "string" then (Sast.StrOp(conv_type e1, Sast.Eqs, conv_type e2), "boolean") else
	 	    (Sast.BinOp(fst e1, Sast.Equal, fst e2), "boolean")
	 | Neq -> if expr_t = "string" then (Sast.StrOp(conv_type e1, Sast.Neqs, conv_type e2), "boolean") else
	 	  (Sast.BinOp(fst e1, Sast.Neq, fst e2), "boolean")
	 | Less -> if expr_t = "string" then raise (Failure ("type error")) else
	 	   (Sast.BinOp(fst e1, Sast.Less, fst e2), "boolean")
	 | LessEq -> if expr_t = "string" then raise (Failure ("type error")) else
	 	     (Sast.BinOp(fst e1, Sast.LessEq, fst e2), "boolean")
	 | Grt -> if expr_t = "string" then raise (Failure ("type error")) else
	 	  (Sast.BinOp(fst e1, Sast.Grt, fst e2), "boolean")
	 | GrtEq -> if expr_t = "string" then raise (Failure ("type error")) else
	 	    (Sast.BinOp(fst e1, Sast.GrtEq, fst e2), "boolean")
	 | And -> if expr_t = "string" then raise (Failure ("type error")) else
	 	  (Sast.BinOp(fst e1, Sast.And, fst e2), "boolean")
	 | Or -> if expr_t = "string" then raise (Failure ("type error")) else
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
		let expr = get_expr_with_type env e t in
		if t = "string" then (Sast.AssignStr(id, (conv_type (expr,t))), t)
		else Sast.Assign(id, expr), t

	| AssignSet(id, subs, i, e) ->
		if (get_vartype env id) != "string" then raise (Failure ("type error"))
		else let index = get_expr_with_type env i "int"
		     and expr = check_expr env e in
		     ( match subs with
			  SubChar -> Sast.AssignSet(id, Sast.SubChar, index, (conv_type expr)), "string"
			| SubInt  -> if (snd expr) != "int" then raise (Failure ("type error"))
				     else Sast.AssignSet(id, Sast.SubInt, index, (fst expr)), "int"
			| SubStr  -> Sast.AssignSet(id, Sast.SubStr, index, (conv_type expr)), "string" )

	| AssignRange(id, i, l, e) ->
		if (get_vartype env id) != "string" then raise (Failure ("type error"))
		else Sast.AssignRange(  id,
					get_expr_with_type env i "int",
					get_expr_with_type env l "int",
					conv_type (check_expr env e)), "string"

	| Extract(id, subs, i) -> Sast.Extract(id, subs, i)
	| Sublen(id, i, len) -> Sast.Sublen(id, i, len)
	| Chset(id, sets, str) -> Sast.Chset(id, sets, str)
	| RemoveSet(id, subs, i) -> Sast.RemoveSet(id, subs, i)
	| RemoveRange(id, i, len) -> Sast.RemoveStr(id, i, len)
	| Stream(strm, dest, e) -> Sast.Stream(strm, dest, e)
	| Call(func, e_list) -> Sast.Call(func, e_list)
	| Fop(fop, e) -> Sast.Fop(fop, e)
	| Noexpr -> (Sast.Noexpr, "void")

(* get expr_t(sast type) by expr(ast type) with given type
 * raise error if the expression type does match requirement *)
and get_expr_with_type env expr t = 
	let e = check_expr env expr in
	if (snd e) != t then raise (Failure ("type error")) else (fst e)


let check_formal env formal = 
	let (s1, s2, expr) = formal in
	let e = check_expr env expr in
		if snd e != s1 && snd e != "void" && s1 != "string" then raise (Failure ("type error"))
	    else let ret = add_local s2 s1 env in if StringMap.is_empty ret then raise (Failure ("local variable " ^ s2 ^ " is already defined")) 
        else if s1 = "string" && (snd e = "int" || snd e = "boolean") then (s1, s2, Sast.ToStr(fst e)) 
	    else (s1, s2, fst e)

let rec check_formals env formals = 
	match formals with 
	  [] -> []
	| hd::tl -> (check_formal env hd) :: (check_formals env tl) 


let rec check_stmt env func = function
	  Block(stmt_list) -> Sast.Block(check_stmt_list env func stmt_list)
	| Decl(s1, s2, expr) -> let e = check_expr env expr in
							if snd e != s1 && snd e != "void" && s1 != "string" then raise (Failure ("type error"))
	        				else let ret = add_local s2 s1 env in if StringMap.is_empty ret then raise (Failure ("local variable " ^ s2 ^ " is already defined")) 
	        				else if s1 = "string" && (snd e = "int" || snd e = "boolean") then Sast.Decl(s1, s2, Sast.ToStr(fst e)) 
							else Sast.Decl(s1, s2, fst e)
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

let check_global env global =
	let (v_type, name, expr) = global in
	let ret = add_global name v_type env in
	if StringMap.is_empty ret then raise (Failure ("global variable " ^ name ^ " is already defined"))
	else (v_type, name, (check_expr env expr))

let rec check_globals env globals = 
	match globals with
	  [] -> []
	| hd::tl -> (check_global env hd) :: (check_globals env tl)

let check_function env func =
	let env.locals = StringMap.empty in
	let ret = add_function func.fname func.returnType env in
	if StringMap.is_empty ret then raise (Failure ("function " ^ func.fname ^ " is already defined"))
	else {Sast.returnType = func.returnType; Sast.fname = func.fname; Sast.formals = (check_formals env func.formals); Sast.body = (check_stmt_list env func func.body)}

let rec check_functions env funcs = 
	match funcs with
	  [] -> []
	| hd::tl -> (check_function env hd) :: (check_functions env tl) 


let check_program (globals, funcs) = 
	let env = {	locals = StringMap.empty;
			globals = StringMap.empty;
			functions = StringMap.empty }
	in
	((check_globals env globals), (check_functions env funcs))
