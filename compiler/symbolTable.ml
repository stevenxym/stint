module StringMap = Map.Make(String)

type env = {
        locals:         string StringMap.t;
	globals:        string StringMap.t;
	functions:      string list StringMap.t;
}

let find_variable name env = 
	try StringMap.find name env.locals
	with Not_found -> try StringMap.find name env.globals 
	with Not_found -> ""
	(*raise (Failure ("undefined variable " ^ name)) *)

let find_function name env = 
	try StringMap.find name env.functions
	with Not_found -> []
	(*raise (Failure ("undefined function " ^ name)) *)

let add_local name v_type env =
	if StringMap.mem name env.locals then StringMap.empty
	(*raise (Failure ("variable " ^ name ^ " is already defined"))*)
	else StringMap.add name v_type env.locals

let add_global name v_type env =
	if StringMap.mem name env.globals then StringMap.empty
	else StringMap.add name v_type env.globals

let add_function name ret_t formals env =
	if StringMap.mem name env.functions then StringMap.empty
	else StringMap.add name ret_t::(List.fold_left ) env.functions

