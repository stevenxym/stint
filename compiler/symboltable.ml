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
	else StringMap.add name v_type env.locals

let add_global name v_type env =
	if StringMap.mem name env.globals then StringMap.empty
	else StringMap.add name v_type env.globals

let get_arg_type = function
	(t, _, _) -> t

let add_function name return_type formals env =
	if StringMap.mem name env.functions then StringMap.empty
	else let f = List.map get_arg_type formals in
	StringMap.add name (return_type::f) env.functions
