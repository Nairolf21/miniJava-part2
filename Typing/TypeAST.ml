open AST
open Type



(* Typing functions *)


let type_type_info exp env = 
	match exp with
	| Class c -> c :: env 
	| Inter -> env

let type_asttype exp env =
	match exp.info with
	| info -> type_type_info exp.info env

let rec type_program type_list env=
	match type_list with
	| h::t -> type_asttype h env :: type_program t env
	| [] -> []

let typing exp env =
	match exp.package with
	| None -> type_program exp.type_list env
	| Some pack -> type_program exp.type_list env