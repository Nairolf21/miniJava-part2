open Exec

(* AST.t -> memory *)
let rec compile_ast ast =
    compile_classes ast.type_list init_memory

and init_memory =
    (* Add class Object to memory *)
    {
        class_desc_list = [object_class_desc];
        meth_table = ["Object_toString"]
    }

and compile_classes type_list mem = 
    match type_list with
    | [] -> mem
    | h :: t -> compile_classes t (compile_asttype type_list h mem)

and compile_asttype type_list asttype mem =
    match asttype.info with 
    | Inter -> mem
    | Class c -> compile_class type_list c asttype.id mem

and compile_class type_list astclass id_class mem =
    if is_astclass_compiled id_class mem then mem
    else 
        begin
            let parent_asttype = find_asttype_by_ref type_list astclass.cparent in
            let mem_with_parent = compile_asttype type_list parent_asttype mem in
            add_astclass_to_memory asttype parent_asttype.id mem_with_parent
        end

and add_astclass_to_memory asttype id_parent mem =
    match asttype with
    | Inter -> mem
    | Class c ->
                let parent_class_desc = find_class_desc_by_ref id_parent mem.class_desc_list in
                match parent_class_desc with
                | None -> mem
                | Some parent_class_desc ->
                let child_class_desc = create_class_desc_with_parent c asttype.id parent_class_desc in
    {
        class_desc_list = mem.class_desc_list @ [child_class_desc];
        meth_table = add_methods child_class_desc.method_names asttype.id mem.meth_table
    
    }

and create_class_desc_with_parent astclass id parent_class_desc =
    let namelist = List.map (fun attr -> attr.aname) astclass.cattributes in
    let methodlist= List.map (fun meth -> meth.mname) astclass.cmethods in
    { 
        name = id; 
        attributes = parent_class_desc.attributs @ namelist; 
        method_names = parent_class_desc.method_names @ methodlist 
    }

and find_class_desc_by_ref id_class class_desc_list =
    match class_desc_list with
    | [] -> None
    | h :: t -> if h.name = id_class then Some h
                else find_class_desc_by_ref id_class t
