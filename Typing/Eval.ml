open AST
open Memory_Model

let rec eval_program mem =
    let main_astmethod = find_main mem.meth_table in
    print_endline "main method found: ";
    print_method "" main_astmethod;
    print_endline "";
    mem

and find_main meth_table =
    List.find is_main (sm_value_list meth_table)

and is_main astmethod =
    if is_method_static astmethod
        && astmethod.mname = "main"
    then
        true
    else
        false

and is_method_static astmethod =
    let is_static_modifier = function
        | Static -> true
        | _ -> false 
    in
    List.exists is_static_modifier astmethod.mmodifiers
