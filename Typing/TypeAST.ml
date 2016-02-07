open AST
open Type



(* Typing functions *)

let typing exp =
	match exp.package with
	| None -> Null
	| Some pack -> String(String.concat "." pack) 


(* Printing functions *)
  
let rec print_tpackage p =
	match p with 
	| [] -> ()
	| [h] -> print_endline ("package'"^h^"'")
	| h::t -> print_string ("package'"^h^"'."); print_tpackage t

let print_typing exp =
  match exp.package with
  | None -> ()
  | Some pack -> print_tpackage pack ;
