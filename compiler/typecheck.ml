open Ast
open Sast
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


