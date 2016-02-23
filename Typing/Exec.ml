open AST
open Type

module StringMap = Map.Make(String)

let sm_list_keys map =
    let rec list_keys_rec assoc_list key_list =
        match assoc_list with
        | [] -> []
        | [(k, _)] -> key_list @ [k]
        | (k, _) :: t -> key_list @ (list_keys_rec t [k]) 
    in
    list_keys_rec (StringMap.bindings map) [];;

(* TODO: Be more specific with the attributes : add modifiers and type ?
 * Or is it to be made by the typing team ?*)
type class_desc = 
    {
        name : string;
        attributes : string list;
        method_names : string StringMap.t
    }

let object_class_desc =
    {
        name = "Object";
        attributes = [];
        method_names = [{ mnp_name = "toString"; mnp_method_table_key = "Object_toString"} ]
    }

let add_method_to_class_desc mname meth_table_key class_desc =
    {
        name = class_desc.name;
        attributes = class_desc.attributes;
        method_names = class_desc.method_names @ [{ mnp_name = mname; mnp_method_table_key = meth_table_key}]
    }


(* Method table entry. Maps the name of the method (ClassName_methodName) 
 * to the AST method representation.
 * Should be created using create_meth_table_entry *)
type meth_table_entry = 
    {
        comp_name : string;
        meth : astmethod
    }

let create_meth_table_entry id_class astmethod =
    {
        comp_name = id_class^"_"^astmethod.mname;
        meth = astmethod
    }

(* Represents the memory structures built to prepare the execution.
 * It is constructed from the ast using the function compile_classes 
 * meth_table: method table. Maps a unique string name for each method and the AST representation of said method.
 * class_desc_list: list storing the class descriptors *)
(* TODO: define the functions to access these lists *)
type memory =
    {
        meth_table : meth_table_entry list;
        class_desc_list : class_desc list
    }
(*
(* Creates an class descriptor based on an AST.astclass. Should be called from 
 * create_class_desc_list. *)
(* astclass -> string -> class_desc *)
let create_class_desc astclass id = 
    let namelist = List.map (fun attr -> attr.aname) astclass.cattributes in
    let methodlist= List.map (fun meth -> meth.mname) astclass.cmethods in
    { name = id; attributes = namelist; method_names = methodlist }

(* AST.astclass -> string -> meth_table_entry list *)
let rec add_methods class_meth_list id_class meth_table =
    match class_meth_list with 
    | [] -> meth_table
    | h :: t -> add_methods t id_class (meth_table @ [create_meth_table_entry id_class h])

(*and add_methods*)
*)
let rec find_asttype_by_ref type_list ref =
    match type_list with
    | h :: t -> if h.id = ref.tid then Some h 
                else find_asttype_by_ref t ref
    | [] -> None
(*
and find_astclass_by_ref type_list ref =
    let asttype = find_asttype_by_ref type_list ref in
    match asttype with
    | None -> None
    | Some a -> match a.info with
                | Inter -> None
                | Class c -> Some c

let find_class_desc_in_memory mem class_id =
    let rec find_class_desc_rec class_desc_list class_id =
        match class_desc_list with
        | [] -> None
        | h :: t -> if h.name = class_id then Some h
                    else find_class_desc_rec t class_id
    in
    find_class_desc_rec mem.class_desc_list class_id
    


let rec add_classes_to_memory type_list mem =
    match type_list with 
    | [] -> mem
    | h :: t -> match h.info with 
                | Inter -> mem
                | Class c -> add_classes_to_memory t (add_class_to_memory c h.id mem)

 

and is_asttype_compiled asttype mem =
    let rec is_asttype_in_class_desc_list class_desc_list asttype =
    match class_desc_list with 
    | [] -> false
    | h :: t -> if h.name = asttype.id then true
                else is_asttype_in_class_desc_list t asttype
    in
    is_asttype_in_class_desc_list mem.class_desc_list asttype

(* AST.t -> string -> memory *)
and add_class_to_memory astclass id_class mem =
    print_string "treating class: ";
    print_endline id_class;
    print_string "parent ref_type: ";
    print_endline (stringOf_ref astclass.cparent);
    {
        meth_table =  add_methods astclass.cmethods id_class mem.meth_table;
        class_desc_list = mem.class_desc_list @ [create_class_desc astclass id_class]
    }

and compile_parent type_list astclass id_class mem =
    let parent_class = find_asttype_by_ref type_list (astclass.cparent) in
    match parent_class with 
    | None -> mem
    | Some t -> match t.info with
                | Inter -> mem
                | Class c -> add_class_to_memory c t.id mem

(* TODO: Inheritance *)
let compile_classes ast = 
    add_classes_to_memory ast.type_list { meth_table = []; class_desc_list = [] }

*)

(*Printing functions *)

let print_method_name_map m_name mt_name =
    print_string m_name;
    print_string " -> ";
    print_endline mt_name

let print_class_desc cd = 
    print_endline "";
    print_endline ("name: "^cd.name);
    print_endline "Attrbutes:";
    List.iter (fun el -> print_endline el) cd.attributes;
    print_endline "Methods:";
    StringMap.iter print_method_name_map cd.method_names

let print_class_desc_list cdl =
    print_endline "";
    print_endline "Class descriptors";
    print_endline "";
    List.iter (function el -> print_class_desc el) cdl;
    print_endline ""

let print_method_table_entry entry =
    print_string entry.comp_name;
    print_string ": ";
    print_string entry.meth.mname;
    print_endline ""

let print_method_table meth_table =
    print_endline "Method table (comp_name: ast_name)";
    print_endline "";
    List.iter (function el -> print_method_table_entry el) meth_table;
    print_endline ""

let print_memory mem = 
    print_endline "";
    print_endline "Printing memory representation";
    print_endline "";
    print_method_table mem.meth_table;
    print_class_desc_list mem.class_desc_list;
    print_endline "";
    print_endline "End of memory representation";
    print_endline ""

