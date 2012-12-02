(* output
 	-takes simple IR produced by the simlification pass
	-outputs C++ code
*)

open Ast
open Check

let depth = ref 0

let rec tabs_helper = function
	0 -> ""
	| x -> "  " ^ tabs_helper (x-1)
	
let rec tabs = function
	0 -> tabs_helper depth.contents
	| x -> ""