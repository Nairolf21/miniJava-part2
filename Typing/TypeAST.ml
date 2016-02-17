open AST
open Type

(* packages seem not be stored by the parser
 * Maybe it's not necessary to verify packages *)
let check_package package = 
    match package with
    | Some p -> print_endline "package found"
    | None -> print_endline "no package found"
    | _ -> print_endline "default result"

let check_type_list type_list = 
    match type_list with 
    | [] -> print_endline "no type_list"
    | h::t -> print_endline "list found"
    | _ -> print_endline "default result"

let typing ast = 
    print_endline "type-checking in progress";
    check_type_list ast.type_list;
    check_package ast.package
