open AST
open Pervasives
open Memory_Model

(* AST.t -> memory *)
let rec compile_ast ast =
    compile_classes ast.type_list init_memory

and init_memory =
    (* Add class Object to memory *)
    {
        class_desc_list = [object_class_desc];
        meth_table = StringMap.empty (* For the moment, the mapping is done with an astmethod, which cannot be done for Object *)
    }

and compile_classes type_list mem = 
    match type_list with
    | [] -> mem
    | h :: t -> compile_classes t (compile_asttype type_list h mem)

and compile_asttype type_list asttype mem =
    match asttype.info with 
    | Inter -> mem
    | Class c -> compile_class type_list c asttype.id mem

and compile_class type_list astclass class_id mem =
    if is_astclass_compiled class_id mem then mem
    else 
        begin
            let parent_asttype = find_asttype_by_ref type_list astclass.cparent in
            add_astclass_to_memory astclass class_id astclass.cparent.tid (mem_with_parent type_list parent_asttype astclass.cparent.tid mem)
        end

and mem_with_parent type_list parent_asttype parent_id mem =
    match parent_asttype with
    | None -> if parent_id = "Object" then mem
              else failwith (parent_id^"not found in the AST")
    | Some parent_asttype -> compile_asttype type_list parent_asttype mem

and is_astclass_compiled class_id mem =
    let rec is_astclass_compiled_rec class_id class_desc_list =
    match class_desc_list with 
    | [] -> false
    | h :: t -> if h.name = class_id then true
                else is_astclass_compiled_rec class_id t
    in
    is_astclass_compiled_rec class_id mem.class_desc_list
    

and add_astclass_to_memory astclass class_id parent_id mem =
    let parent_class_desc = find_class_desc_by_ref parent_id mem.class_desc_list in
    let parent_mmap = parent_class_desc.method_names in
    let child_mlist = method_name_list_of_astclass astclass in
    let (redefined_mlist, inherited_mmap) = add_parent_methods_to_mmap parent_mmap child_mlist parent_id class_id in
    let (child_mmap, updated_meth_table) = add_child_methods_to_mmap redefined_mlist inherited_mmap class_id astclass mem.meth_table in 

    let child_class_desc = create_child_class_desc parent_class_desc class_id (attribute_list_of_astclass astclass) child_mmap in

    {
        meth_table = updated_meth_table;
        class_desc_list = mem.class_desc_list @ [child_class_desc]
    
    }

and add_child_methods_to_mmap child_mlist child_mmap child_id child_astclass meth_table =
    print_endline ("add_child_methods for class "^child_id);
    print_string "child_mlist: ";
    List.iter (fun el -> print_string (el^";")) child_mlist;
    print_endline "";
    print_endline "";
    match child_mlist with
    | [] -> child_mmap, meth_table
    | h :: t -> 
            let meth_table_key = full_mname h child_id in
            let astmethod = find_astmethod_by_name h child_astclass in
            let updated_meth_table = StringMap.add meth_table_key astmethod meth_table in
            
            add_child_methods_to_mmap t (StringMap.add h meth_table_key child_mmap) child_id child_astclass updated_meth_table

(*Construct method map of the child class descriptor, adding only methods inherited but not redefined by the child.
 * Returns too elements: (string list) * (string StringMap.t) => (child_redefined_method_list, parent_method_mmap)*)
and add_parent_methods_to_mmap parent_mmap child_mlist parent_id child_id =
    let rec add_parent_methods_rec parent_mlist child_mlist child_mmap =
        match parent_mlist with
        | [] -> (child_mlist, child_mmap)
        | h :: t -> 
                if ListII.is_in_list h child_mlist then add_parent_methods_rec t child_mlist child_mmap
                else
                let new_child_list = ListII.remove h child_mlist in
                let method_table_key = construct_meth_table_key h parent_mmap child_id child_mlist in
                let updated_child_mmap = StringMap.add h method_table_key child_mmap in
                add_parent_methods_rec t new_child_list updated_child_mmap 
    in
    add_parent_methods_rec (sm_list_keys parent_mmap) child_mlist StringMap.empty

and  construct_meth_table_key method_name parent_mmap child_id child_mlist =
    if ListII.is_in_list method_name child_mlist then full_mname method_name child_id 
    else StringMap.find method_name parent_mmap

and find_class_desc_by_ref class_id class_desc_list =
    match class_desc_list with
    | [] -> Pervasives.failwith "find_class_desc_by_ref: class_desc not found"
    | h :: t -> if h.name = class_id then h
                else find_class_desc_by_ref class_id t

