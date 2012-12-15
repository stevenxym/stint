open Ast
open Symboltable

module StringMap = Map.Make(String)

(* get the type of expression:
 *  -> string if one of the two operands having string type
 *  -> int/boolean if both of the operands having the same type *)
let get_expr_type t1 t2 =
	if t1 = "void" || t2 = "void" then raise (Failure ("cannot use void type inside expression")) else
	if t1 = "string" || t2 = "string" then "string" else
	if t1 = "int" && t2 = "int" then "int" else
	if t1 = "boolean" && t2 = "boolean" then "boolean" else
	raise (Failure ("type error"))

(* mark int & boolean expression to string type *)
let conv_type = function 
	(expr, t) -> if t = "void" then raise (Failure ("cannot use void type inside expression")) else
		     if not(t = "string") then Sast.ToStr(expr) else expr

(* get variable type according to the name
 * raise error if no name matching in variable list *)
let get_vartype env id = 
	let t = find_variable id env in
	if t = "" then raise (Failure ("undefined variable " ^ id)) else t

let check_string_id expr = 
	let (e, t) = expr in
	if not(t = "string") then raise (Failure ("type error")) else
	( match e with Sast.Id(i) -> e
			| _ -> raise (Failure ("should use identifier")) )

(* check the expression type can be used for
 * the corresponding argument according to definition
 * return the new expression list in expr_t for sast *)
let check_func_arg lst expr arg_t =
	if arg_t = "string" then (conv_type expr)::lst else
	if (snd expr) = arg_t then (fst expr)::lst else
	raise (Failure("unmatched argument type"))

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
	  Add -> (Sast.StrOpAt((conv_type e1), Sast.Adds, (conv_type e2), pos), "string")
	  | Sub -> (Sast.StrOpAt((conv_type e1), Sast.Subs, (conv_type e2), pos), "string")
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
		if not((get_expr_type (snd fst_expr) (snd snd_expr)) = "string") then raise (Failure ("type error"))

		else match_str_oper fst_expr op snd_expr position

	| Assign(id, e) ->
		let t = get_vartype env id in
		if t = "string" then 
		     Sast.AssignStr(id, (conv_type (check_expr env e))), "void"
		else Sast.Assign(id, (get_expr_with_type env e t)), "void"

	| AssignSet(id, subs, i, e) ->
		if not((get_vartype env id) = "string") then raise (Failure ("type error"))
		else let index = get_expr_with_type env i "int"
		     and expr = check_expr env e in
		     ( match subs with
			  SubChar -> Sast.AssignSet(id, Sast.SubChar, index, (conv_type expr)), "void"
			| SubInt  -> if not((snd expr) = "int") then raise (Failure ("type error"))
				     else Sast.AssignSet(id, Sast.SubInt, index, (fst expr)), "void"
			| SubStr  -> Sast.AssignSet(id, Sast.SubStr, index, (conv_type expr)), "void" )

	| AssignRange(id, i, l, e) ->
		if not((get_vartype env id) = "string") then raise (Failure ("type error"))
		else Sast.AssignRange(  id,
					get_expr_with_type env i "int",
					get_expr_with_type env l "int",
					conv_type (check_expr env e)), "void"

	| Extract(id, subs, i) ->
		if not((get_vartype env id) = "string") then raise (Failure ("type error"))
		else let index = get_expr_with_type env i "int" in
		     ( match subs with
			  SubChar -> Sast.Extract(id, Sast.SubChar, index), "string"
			| SubInt  -> Sast.Extract(id, Sast.SubInt, index), "int"
			| SubStr  -> Sast.Extract(id, Sast.SubStr, index), "string" )

	| Sublen(id, i, l) ->
		if not((get_vartype env id) = "string") then raise (Failure ("type error"))
		else Sast.Sublen( id,
				  get_expr_with_type env i "int",
				  get_expr_with_type env l "int"), "string"

	| Chset(id, sets, s) ->
		if not((get_vartype env id) = "string") then raise (Failure ("type error"))
		else let str = get_expr_with_type env s "string" in
		     ( match sets with
			  Spl -> Sast.Chset(id, Sast.Spl, str), "int"
			| Fnd -> Sast.Chset(id, Sast.Fnd, str), "int" )
			
	| RemoveSet(id, subs, i) ->
		if not((get_vartype env id) = "string") then raise (Failure ("type error"))
		else let index = get_expr_with_type env i "int" in
		     ( match subs with
			  SubChar -> Sast.RemoveSet(id, Sast.SubChar, index), "void"
			| SubInt -> Sast.RemoveSet(id, Sast.SubInt, index), "void"
			| SubStr -> Sast.RemoveSet(id, Sast.SubStr, index), "void" )

	| RemoveRange(id, i, l) ->
		if not((get_vartype env id) = "string") then raise (Failure ("type error"))
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

	| Call(func, el) ->
		let args = find_function func env in	(* return & arguments type list from definition *)
		( match args with
			[] -> raise (Failure ("undefined function " ^ func))
			| hd::tl -> let new_list = try List.fold_left2 check_func_arg [] (List.map (check_expr env) el) tl
						   with Invalid_argument "arg" -> raise(Failure("unmatched argument list"))
				    in Sast.Call(func, List.rev new_list ), hd )

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
	if not((snd e) = t) then raise (Failure ("type error")) else (fst e)


(* modified: just simply do not allow to assign value in function definition*)
let check_formal env formal = 
	let (t, name, expr) = formal in
	let ret = add_local name t env in
	if t = "void" then raise (Failure("cannot use void as variable type")) else
	if StringMap.is_empty ret then raise (Failure ("local variable " ^ name ^ " is already defined"))
	else let env = {locals = ret; globals = env.globals; functions = env.functions } in
	( match expr with
		Noexpr -> (t, name, Sast.Noexpr), env
		| _ -> raise(Failure("cannot assign value inside function definition")) )
	(*let e = check_expr env expr i
		if not(snd e = s1) && not(snd e = "void") && not(s1 = "string") then raise (Failure ("type error"))
	    else let ret = add_local s2 s1 env in if StringMap.is_empty ret then raise (Failure ("local variable " ^ s2 ^ " is already defined")) 
        else if s1 = "string" && (snd e = "int" || snd e = "boolean") then (s1, s2, Sast.ToStr(fst e)) 
	    else (s1, s2, fst e)*)
	

let rec check_formals env formals = 
	match formals with 
	  [] -> []
	| hd::tl -> let f, e = (check_formal env hd) in (f, e)::(check_formals e tl) 


let rec check_stmt env func = function
	  Block(stmt_list) -> (Sast.Block(check_stmt_list env func stmt_list)), env
	| Decl(s1, s2, expr) -> let e = check_expr env expr in
				(*modified: 1. check s1 cannot be void; 2. expr can be void*)
							if s1 = "void" then raise (Failure("cannot use void as variable type")) else
							if not(snd e = s1) && not(snd e = "void") && not(s1 = "string") then raise (Failure ("type error"))
	        				else let ret = add_local s2 s1 env in 
	        				if StringMap.is_empty ret then raise (Failure ("local variable " ^ s2 ^ " is already defined")) 
	        				else let env = {locals = ret; globals = env.globals; functions = env.functions } in
	        				if s1 = "string" && (snd e = "int" || snd e = "boolean") then (Sast.Decl(s1, s2, Sast.ToStr(fst e))), env 
							else (Sast.Decl(s1, s2, fst e)), env
	| Expr(expr) -> (Sast.Expr(fst (check_expr env expr))), env
	| Return(expr) -> let e = check_expr env expr in
					  if not(snd e = func.returnType) then raise (Failure ("The return type doesn't match!"))
					  else (Sast.Return(fst e)), env 
	| If(expr, stmt1, stmt2) ->	let e = check_expr env expr in
								if not(snd e = "boolean") then raise (Failure ("The type of the condition in If statement must be boolean!")) 
								else (Sast.If(fst e, fst (check_stmt env func stmt1), fst (check_stmt env func stmt2))), env	(* if() {} else{} *)
	| While(expr, stmt) -> let e = check_expr env expr in
						   if not (snd e = "boolean") then raise (Failure ("The type of the condition in While statement must be boolean!"))
						   else (Sast.While(fst e, fst (check_stmt env func stmt))), env				(* while() {} *)
	| Break -> (Sast.Break), env

and check_stmt_list env func = function 
	  [] -> []
	(* | [s] -> (match s with Return(expr) -> [fst (check_stmt env func s)]
						| _ -> raise (Failure ("The last statement must be return statement"))) *)
	| hd::tl -> let s,e = (check_stmt env func hd) in s::(check_stmt_list e func tl)

let check_global env global =
	let (v_type, name, expr) = global in
	let ret = add_global name v_type env in
	if StringMap.is_empty ret then raise (Failure ("global variable " ^ name ^ " is already defined"))
	else let env = {locals = env.locals; globals = ret; functions = env.functions } in
	(v_type, name, fst (check_expr env expr)), env

let rec check_globals env globals = 
	match globals with
	  [] -> []
	| hd::tl -> let g, e = (check_global env hd) in (g, e)::(check_globals e tl)

let check_function env func =
	if List.length func.body = 0 then raise (Failure ("The last statement must be return statement"))
	else 
	match List.hd (List.rev func.body) with
	  Return(_) ->
	  	let env = {locals = StringMap.empty; globals = env.globals; functions = env.functions } in
		let ret = add_function func.fname func.returnType func.formals env in
		if StringMap.is_empty ret then raise (Failure ("function " ^ func.fname ^ " is already defined"))
		else let env = {locals = env.locals; globals = env.globals; functions = ret } in
		let f = check_formals env func.formals in
		let formals = List.map (fun formal -> fst formal) f in
		(match f with
		[] -> let body = check_stmt_list env func func.body in
			{Sast.returnType = func.returnType; Sast.fname = func.fname; Sast.formals = formals; Sast.body = body}, env
		| _ -> 	let e = snd (List.hd (List.rev f)) in
			let body = check_stmt_list e func func.body in
			{Sast.returnType = func.returnType; Sast.fname = func.fname; Sast.formals = formals; Sast.body = body}, e )
	  | _ -> raise (Failure ("The last statement must be return statement"))



let rec check_functions env funcs = 
	match funcs with
	  [] -> []
	| hd::tl -> let f, e = (check_function env hd) in f::(check_functions e tl) 


let check_program (globals, funcs) = 
(* 	let ret = add_function "readFile" "string" [("string", _, _)] StringMap.empty in
	let ret = add_function "printFile" "boolean" [("string", _, _); ("string", _, _)] ret in 
	let ret = add_function "replaceAll" "string"  *)
 	let env = {	locals = StringMap.empty;
			globals = StringMap.empty;
			functions = StringMap.empty }
	in
	let g = check_globals env globals in
	let globals = List.map (fun global -> fst global) g in
	match g with
	 [] -> (globals, (check_functions env (List.rev funcs)))
	| _ -> let e = snd (List.hd (List.rev g)) in (globals, (check_functions e (List.rev funcs)))
	
	


