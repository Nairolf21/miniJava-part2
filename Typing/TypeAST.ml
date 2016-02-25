open AST
open Type



(* Typing functions *)
let add_class_list c env=
	env @ c

let type_type_info exp env = 
	match exp.info with
	| Class c -> add_class_list c env
	| Inter -> ()

let type_astype_info exp env =
	match exp.info with
	| Some info -> type_type_info exp env

let rec type_program exp env=
	match exp.type_list with
	| None -> ()
	| h::t -> type_asttype_info h env :: type_program t env

let typing exp env =
	match exp.package with
	| None -> type_program exp env
	| Some pack -> type_program exp env


(* Printing functions *)
(*   
let rec print_tpackage p =
	match p with 
	| [] -> ()
	| [h] -> print_endline ("package"^h)
	| h::t -> print_string ("package"^h^"."); print_tpackage t

let print_typing exp =
  match exp.package with
  | None -> ()
  | Some pack -> print_tpackage pack ;
 *)