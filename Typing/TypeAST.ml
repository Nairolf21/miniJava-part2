open AST
open Type




(* Typing functions *)

let typing exp =
	match exp.package with
	| None -> Void
(* 	| Some pack -> type_package pack ; List.iter (fun x -> type_type x ) exp.type_list
 *)




(* Printing functions *)

let print_typing exp =
	match exp.package with
	| None -> print_endline "No package"
(* 	| Some pack -> type_package pack ; List.iter (fun x -> print_type_type x ) exp.type_list
 *)