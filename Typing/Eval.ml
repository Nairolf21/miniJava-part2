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
    eval_method main_astmethod mem

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
    let mem = push_to_callstack (Method ast_method) mem in
    eval_statement_list ast_method.mbody mem
    
and eval_statement_list stmt_list mem =
    match stmt_list with
    | [] -> mem
    | h :: t -> eval_statement_list t (eval_statement h mem)

(* AST.statement -> Memory_Model.memory -> Memory_Model.memory *)
and eval_statement statement mem =
    print_statement "" statement;
    match statement with
    | VarDecl var_decl_list -> eval_var_decl_list var_decl_list mem
    | Block stmt_list -> eval_statement_list stmt_list mem
    | Nop -> mem
    | While (expr, stmt) -> eval_while expr stmt mem
    | For _ -> Pervasives.failwith "eval_for not yet implemented"
    | Expr ex -> (eval_expr ex mem).ee_memory
    | _ -> Pervasives.failwith "statement not yet implemented"

and eval_var_decl_list var_decl_list mem =
    match var_decl_list with
    | [] -> mem
    | h :: t -> eval_var_decl_list t (eval_var_decl h mem)

and eval_var_decl var_decl mem =
    match var_decl with
    | (ty, var_name, None) -> push_to_callstack (Variable (init_v_value var_name ty None)) mem 
    | (ty, var_name, Some init_expr) -> 
            let ee = eval_expr init_expr mem in
            let init_value = init_v_value var_name ty (Some ee.ee_value) in
            push_to_callstack (Variable init_value) ee.ee_memory

and eval_while expr stmt mem =
    Pervasives.failwith "eval_while not yet implemented"

and eval_expr expr mem =
    match expr.edesc with 
    | New (ta, cip, ael) -> eval_new ta cip ael mem
    | Val v -> { ee_value = (mm_value_from_ast_value v); ee_memory =  mem }
    | Name n -> eval_name n mem
    | Op (e1, op, e2) -> eval_op e1 e2 op mem
    | _ -> Pervasives.failwith "Eval expr: expression eval not yet implemented"

and eval_new type_arguments class_id_path arg_expr_list mem =
    match type_arguments with
    | Some ta -> Pervasives.failwith "New with type arguments is not supported"
    | None ->
            match class_id_path, arg_expr_list with
            | (cip, []) -> eval_new_without_args cip mem
            | (cip, _) -> Pervasives.failwith "New with args not yet implemented"

(* Simplest form of instantiation: new ClassName().
 * Returns a not yet named obj_desc, and not added to memory*) 
and eval_new_without_args class_id_path mem =
    match class_id_path with
    | [] -> Pervasives.failwith "Class instantiation without a name"
    | [class_id] -> { 
            ee_value = VRefType (Some {vref_class_id = class_id; vref_obj_id = "" }); 
            ee_memory = mem }
    | h :: t -> Pervasives.failwith "Composed class_id not supported"

(* First look in stack. If not in stack, look in heap. *)
(* For the moment, only implementing stack *)
and eval_name name mem =
    let vv = find_value_in_stack name mem in
    print_v_value vv;
    { ee_value = vv.v_value ; ee_memory = mem }



(* Binary operations. Bitwise op and shift are left un implemented for lack of time *)
and eval_op e1 e2 op mem =
    let ee1 = eval_expr e1 mem in
    let ee2 = eval_expr e2 ee1.ee_memory in
    let result v1 v2 op =
        match op with
        | Op_add -> eval_add v1 v2
        | Op_sub -> eval_sub v1 v2
        | Op_mul -> eval_mul v1 v2
        | Op_div -> eval_div v1 v2
        | Op_mod -> eval_mod v1 v2
        | Op_cor -> eval_cor v1 v2
        | Op_cand -> eval_cand v1 v2
        | Op_gt -> eval_gt v1 v2
        | Op_lt -> eval_lt v1 v2
        | Op_ge -> eval_ge v1 v2
        | Op_le -> eval_le v1 v2
        | Op_eq -> eval_eq v1 v2
        | Op_ne -> eval_ne v1 v2
        | _ -> Pervasives.failwith "inline_op eval not yet implemented"
    in
    { 
        ee_value = (result ee1.ee_value ee2.ee_value op);
        ee_memory = ee2.ee_memory
    }


(* Numeric operators *)
and eval_add v1 v2 = 
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VInt (Some (v1 + v2))
    | (VFloat Some v1), (VFloat Some v2) -> VFloat (Some (v1 +. v2))
    | (VInt Some v1), (VFloat Some v2) -> VFloat (Some ((float_of_int v1) +. v2))
    | (VFloat Some v1), (VInt Some v2) -> VFloat (Some (v1 +. (float_of_int v2)))
    | _ -> Pervasives.failwith "eval_add: incompatible types"

and eval_sub v1 v2 = 
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VInt (Some (v1 - v2))
    | (VFloat Some v1), (VFloat Some v2) -> VFloat (Some (v1 -. v2))
    | (VInt Some v1), (VFloat Some v2) -> VFloat (Some ((float_of_int v1) -. v2))
    | (VFloat Some v1), (VInt Some v2) -> VFloat (Some (v1 -. (float_of_int v2)))
    | _ -> Pervasives.failwith "eval_sub: incompatible types"

and eval_mul v1 v2 = 
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VInt (Some (v1 * v2))
    | (VFloat Some v1), (VFloat Some v2) -> VFloat (Some (v1 *. v2))
    | (VInt Some v1), (VFloat Some v2) -> VFloat (Some ((float_of_int v1) *. v2))
    | (VFloat Some v1), (VInt Some v2) -> VFloat (Some (v1 *. (float_of_int v2)))
    | _ -> Pervasives.failwith "eval_mul: incompatible types"

and eval_div v1 v2 = 
    (match v2 with
    | VInt (Some 0) | VFloat (Some 0.) -> Pervasives.failwith "Divsion by 0"
    | _ -> ()
    );
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VInt (Some (v1 / v2))
    | (VFloat Some v1), (VFloat Some v2) -> VFloat (Some (v1 /. v2))
    | (VInt Some v1), (VFloat Some v2) -> VFloat (Some ((float_of_int v1) /. v2))
    | (VFloat Some v1), (VInt Some v2) -> VFloat (Some (v1 /. (float_of_int v2)))
    | _ -> Pervasives.failwith "eval_div: incompatible types"

and eval_mod v1 v2 = 
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VInt (Some (v1 mod v2))
    | _ -> Pervasives.failwith "eval_mod: incompatible types"

(* Conditional boolean operations *)
and eval_cor v1 v2 =
    match v1, v2 with
    | (VBoolean Some v1), (VBoolean Some v2) -> VBoolean (Some (v1 || v2))
    | _ -> Pervasives.failwith "Cannot apply cor on non booleans"

and eval_cand v1 v2 =
    match v1, v2 with
    | (VBoolean Some v1), (VBoolean Some v2) -> VBoolean (Some (v1 && v2))
    | _ -> Pervasives.failwith "Cannot apply cand on non booleans"

(* Comparison operators *)

and eval_gt v1 v2 = 
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VBoolean (Some (v1 > v2))
    | (VFloat Some v1), (VFloat Some v2) -> VBoolean (Some (v1 > v2))
    | (VInt Some v1), (VFloat Some v2) -> VBoolean (Some ((float_of_int v1) > v2))
    | (VFloat Some v1), (VInt Some v2) -> VBoolean (Some (v1 > (float_of_int v2)))
    | _ -> Pervasives.failwith "eval_gt: incompatible types"

and eval_lt v1 v2 = 
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VBoolean (Some (v1 < v2))
    | (VFloat Some v1), (VFloat Some v2) -> VBoolean (Some (v1 < v2))
    | (VInt Some v1), (VFloat Some v2) -> VBoolean (Some ((float_of_int v1) < v2))
    | (VFloat Some v1), (VInt Some v2) -> VBoolean (Some (v1 < (float_of_int v2)))
    | _ -> Pervasives.failwith "eval_lt: incompatible types"

and eval_ge v1 v2 = 
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VBoolean (Some (v1 >= v2))
    | (VFloat Some v1), (VFloat Some v2) -> VBoolean (Some (v1 >= v2))
    | (VInt Some v1), (VFloat Some v2) -> VBoolean (Some ((float_of_int v1) >= v2))
    | (VFloat Some v1), (VInt Some v2) -> VBoolean (Some (v1 >= (float_of_int v2)))
    | _ -> Pervasives.failwith "eval_ge: incompatible types"

and eval_le v1 v2 = 
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VBoolean (Some (v1 <= v2))
    | (VFloat Some v1), (VFloat Some v2) -> VBoolean (Some (v1 <= v2))
    | (VInt Some v1), (VFloat Some v2) -> VBoolean (Some ((float_of_int v1) <= v2))
    | (VFloat Some v1), (VInt Some v2) -> VBoolean (Some (v1 <= (float_of_int v2)))
    | _ -> Pervasives.failwith "eval_le: incompatible types"

and eval_eq v1 v2 = 
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VBoolean (Some (v1 = v2))
    | (VFloat Some v1), (VFloat Some v2) -> VBoolean (Some (v1 = v2))
    | (VInt Some v1), (VFloat Some v2) -> VBoolean (Some ((float_of_int v1) = v2))
    | (VFloat Some v1), (VInt Some v2) -> VBoolean (Some (v1 = (float_of_int v2)))
    | _ -> Pervasives.failwith "eval_eq: incompatible types"

and eval_ne v1 v2 = 
    match v1, v2 with
    | (VInt Some v1), (VInt Some v2) -> VBoolean (Some (v1 <> v2))
    | (VFloat Some v1), (VFloat Some v2) -> VBoolean (Some (v1 <> v2))
    | (VInt Some v1), (VFloat Some v2) -> VBoolean (Some ((float_of_int v1) <> v2))
    | (VFloat Some v1), (VInt Some v2) -> VBoolean (Some (v1 <> (float_of_int v2)))
    | _ -> Pervasives.failwith "eval_ne: incompatible types"

and mm_value_from_ast_value = function 
    | String s -> Pervasives.failwith "String is not yet implemented in Memory Model"
    | Int i_s -> VInt (Some (int_of_string i_s))
    | Float f_s -> VFloat (Some (float_of_string f_s))
    | Char c_s -> VChar c_s
    | Null -> Pervasives.failwith "Null is not yet implemented in Memory Model"
    | Boolean b -> VBoolean (Some b)
