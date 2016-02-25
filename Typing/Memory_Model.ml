(* This module defines the structures used for the memory management.
 * class_desc : Used to describe a class, with information extended from its parent. *)

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
    list_keys_rec (StringMap.bindings map) []


let full_mname mname classname = classname ^ "_" ^ mname

let add_method_to_mmap class_id method_name mmap =
    StringMap.add method_name (full_mname method_name class_id) mmap

type class_desc = 
    {
        name : string;
        attributes : (string * AST.astattribute) list; (*change to an ordered list of cattributes to be able to take type, modifiers etc...*)
        method_names : string StringMap.t
    }

let object_class_desc =
    let mmap = add_method_to_mmap "Object" "toString" StringMap.empty in
    {
        name = "Object";
        attributes = [];
        method_names = mmap
    }


let rec create_child_class_desc parent_class_desc child_id child_attr_list child_mmap = 
    {
        name = child_id;
        attributes = create_child_attribute_list parent_class_desc.attributes child_attr_list;
        method_names = child_mmap
    
    }


and create_child_attribute_list parent_attributes child_attributes =
    match parent_attributes with
    | [] -> child_attributes
    | h :: t -> h :: create_child_attribute_list t (ListII.remove h child_attributes) 


(* Values (instances, variable values, heap) *)
type value =
    | VBoolean of bool option
    | VChar of char option
    | VInt of int option
    | VFloat of float option
    | VRefType of obj_desc option
    | VArray of (value list) option


(* Variable value: can be used for attributes and for stack variables *)
and v_value = { v_name : string; v_value : value }

and obj_desc =
    {
        class_id : string;
        ob_id : string;
        ob_attributes : v_value list
    }

type memory = {
    class_desc_list : class_desc list;
    meth_table : AST.astmethod StringMap.t;
    
    heap : obj_desc list
}

let rec init_v_value name value_type init_value =
    match init_value with
    | None -> { v_name = name; v_value = empty_value value_type }
    | Some init_value -> Pervasives.failwith "init_v_value with with an init value is not yet defined"

and empty_value = function
    | Void -> Pervasives.failwith "Void is not a variable type"
    | Array (array_type, length) -> empty_array_value array_type length
    | Primitive p_type -> empty_primitive_value p_type
    | Ref r_type -> empty_ref_type r_type

and empty_primitive_value = function
    | Boolean -> VBoolean None
    | Char -> VChar None 
    | Byte | Short | Int | Long -> VInt None
    | Long | Float -> VFloat None

and empty_array_value array_type length = VArray None (* I don't know yet if it's enough to init as an empty array *)

and empty_ref_type r_type = VRefType None


let rec new_object class_desc ob_name mem =
    let new_obj = {
        class_id = class_desc.name;
        ob_id = ob_name;
        ob_attributes = create_attribute_value_list class_desc.attributes []
    } in
    add_to_heap new_obj mem

and create_attribute_value_list cd_attribute_list attribute_value_list =
    match cd_attribute_list with
    | [] -> attribute_value_list
    | (n, astattribute) :: t -> create_attribute_value_list t (attribute_value_list @ [(init_v_value n astattribute.atype None)])

and add_to_heap obj_desc mem =
    {
        class_desc_list = mem.class_desc_list;
        meth_table = mem.meth_table;
        heap = mem.heap @ [obj_desc]
    }


let add_method_to_meth_table method_key astmethod meth_table =
    StringMap.add method_key astmethod meth_table


(*Printing functions *)

(*StringMap print functions*)

(* Print element for string StringMap.t*)
let print_ss_el k v =
    print_endline (k^" -> "^v)

let print_meth_table_line name astmethod =
    print_string (name^" -> ");
    print_method "" astmethod;
    print_endline ""

let print_method_table meth_table =
    print_endline "Method table (comp_name: ast_name)";
    print_endline "";
    StringMap.iter print_meth_table_line meth_table;
    print_endline ""


let print_method_name_map mmap =
    StringMap.iter print_ss_el mmap

let print_attribute_name el =
    match el with
    | (name, _) -> print_endline name
    | _ -> ()

let print_attribute_map el =
    match el with
    | (name, astattribute) ->
        print_string name;
        print_string " -> ";
        print_attribute "" astattribute;
        print_endline ""
    | _ -> ()

let print_class_desc cd = 
    print_endline "";
    print_endline ("name: "^cd.name);
    print_endline "Attrbutes:";
    List.iter print_attribute_map cd.attributes;
    print_endline "Methods:";
    print_method_name_map cd.method_names

let print_class_desc_list cdl =
    print_endline "";
    print_endline "Class descriptors";
    print_endline "";
    List.iter (function el -> print_class_desc el) cdl;
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


