open Ast
module StringMap = Map.Make(String)
	
let find_variable name env = 
	let (locals, globals, _) = env in
	try StringMap.find name locals
	with Not_found -> try StringMap.find name globals 
	with Not_found -> raise (Failure ("undefined variable " ^ name)) 

let find_function name env = 
	let (_, _, functions) = env in
	try StringMap.find name functions
	with Not_found -> raise (Failure ("undefined function " ^ name)) 

let add_local name v_type env =
	let (locals, _, _) = env in
	if StringMap.mem name locals then raise (Failure ("variable " ^ name ^ " is already defined"))
	else StringMap.add name v_type locals

let add_global name v_type env =
	let (_, globals, _) = env in
	if StringMap.mem name globals then raise (Failure ("variable " ^ name ^ " is already defined"))
	else StringMap.add name v_type globals

let add_function name v_type env =
	let (_, _, functions) = env in
	if StringMap.mem name functions then raise (Failure ("function " ^ name ^ " is already defined"))
	else StringMap.add name v_type functions

