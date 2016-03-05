open AST
open Memory_Model

type evaluated_expr = { ee_value : Memory_Model.value; ee_memory : Memory_Model.memory }

let rec eval_program mem =
    let main_astmethod = find_main mem.meth_table in
    print_endline "main method found: ";
    print_method "" main_astmethod;
    print_endline "end main method found";
    print_endline "";
    print_endline "starting eval";
    print_endline "-------------";
    print_endline "";
    mem = eval_method main_astmethod mem

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

and eval_method ast_method mem =
    eval_statement_list ast_method.mbody mem
    
and eval_statement_list stmt_list mem =
    match stmt_list with
    | [] -> mem
    | h :: t -> eval_statement_list t (eval_statement h mem)

and eval_statement statement mem =
    print_statement "" statement;
    match statement with
    | Block stmt_list -> eval_statement_list stmt_list mem
    | Expr ex -> (eval_expr ex mem).ee_memory
    | _ -> Pervasives.failwith "statement not yet implemented"

and eval_expr expr mem =
    match expr.edesc with 
    | Val v -> { ee_value = (mm_value_from_ast_value v); ee_memory =  mem }
    | _ -> Pervasives.failwith "Eval expr: expression eval not yet implemented"

and mm_value_from_ast_value = function 
    | String s -> Pervasives.failwith "String is not yet implemented in Memory Model"
    | Int i_s -> VInt (Some (int_of_string i_s))
    | Float f_s -> VFloat (Some (float_of_string f_s))
    | Char c_s -> VChar c_s
    | Null -> Pervasives.failwith "Null is not yet implemented in Memory Model"
    | Boolean b -> VBoolean (Some b)
