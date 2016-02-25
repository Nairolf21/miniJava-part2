open AST
open Type



(* Typing functions *)


let type_type_info exp env = 
	match exp.info with
	| Class c -> c @ env 
	| Inter -> ()

let type_asttype exp env =
	match exp.info with
	| Some info -> type_type_info exp env
	| None -> ()

let rec type_program type_list env=
	match type_list with
	| None -> ()
	| h::t -> type_asttype h env :: type_program t env

let typing exp env =
	match exp.package with
	| None -> type_program exp.type_list env
	| Some pack -> type_program exp.type_list env