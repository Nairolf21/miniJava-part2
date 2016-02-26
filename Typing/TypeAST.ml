open AST
open Type




(* 1st pass : detection of class declarations, add them to env *)
let type_type_info info id env = 
	match info with
	| Class c -> (id, c) :: env
	| Inter -> env (* We don't consider interfaces here *)

let type_asttype exp env =
	match exp.info with
	| info -> type_type_info exp.info exp.id env


let rec type_program type_list env=
	match type_list with
	| h::t -> type_asttype h env :: type_program t env
	| [] -> []

let typing exp env =
	match exp.package with
	| None -> type_program exp.type_list env
	| Some pack -> type_program exp.type_list env