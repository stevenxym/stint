open Ast
open Sast
open SymbolTable

module StringMap = Map.Make(String)

let check_global_var env var =
	let (v_type, name, value) = var in
	add_global name v_type env;
	(v_type, name, value)

let check_function env funcs = 
	funcs

let check_program (vars, funcs) = 
	let env = {	locals = StringMap.empty;
			globals = StringMap.empty;
			functions = StringMap.empty }
	in
	(List.map (check_global_var env) vars, List.map (check_function env) funcs)


