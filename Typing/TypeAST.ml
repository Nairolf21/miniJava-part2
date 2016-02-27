open AST
open Type
open TypeErrors



(* Find if the class was already declared (ie added to env) *)
let rec class_exists classname env =
		match env with
		[] -> false
		| (id, c):: t -> if id=classname then true (* (id, c) *)
							else class_exists classname t

let rec cparent_in_cenv c cenv=
	match cenv with
	|[] -> false
	| (id, c1) :: t -> if id = c.cparent.tid then true
					else cparent_in_cenv c t


(* Check heritage *)
let rec check_cparent cenv=
	match cenv with
	| [] -> false
	| (id, c):: t -> if ((not (c.cparent.tid="Object")) && (not (cparent_in_cenv c cenv) )) then (raise (Parent_class_unknown(c.cparent.tid)))
						else check_cparent t 

(* 1st pass : detection of class declarations, add them to env *)
let type_type_info info id env = 
	match info with
	| Class c ->  if not (class_exists id env) then (id, c) :: env
					else raise (Class_already_declared(id))
	| Inter -> env (* We don't consider interfaces here *)

let type_asttype exp env = type_type_info exp.info exp.id env
		
(* 	let cenv = type_type_info exp.info exp.id env in *)

let rec class_env type_list env=
	match type_list with
	| [] -> []
	| h::t -> type_asttype h env :: class_env t (type_asttype h env)


let typing exp env = 
	let classenv = class_env exp.type_list env in 
		if (check_cparent classenv) then (print_endline "heritage ok");


(* 	
	let classenv = class_env exp.type_list env in
		match exp.package with
		| None -> classenv
		| Some pack -> classenv *)
