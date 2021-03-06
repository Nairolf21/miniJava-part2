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

let sm_value_list map =
    let rec value_list_rec assoclist =
    match assoclist with
        | [] -> []
        | (k, v) :: t -> v :: (value_list_rec t)
    in
    value_list_rec (StringMap.bindings map)

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
(* Note: it may have been an error to over type values execution side. Maybe a Numeric type for Int and Float would have sufficed *)
type value =
    | VBoolean of bool option
    | VChar of char option
    | VInt of int option
    | VFloat of float option
    | VString of string option
    | VRefType of vref_type option
    | VArray of (value list) option

and vref_type = { vref_obj_id : string; vref_class_id : string }

(* Variable value: can be used for attributes and for stack variables *)
and v_value = { v_name : string; v_value : value }

and obj_desc =
    {
        class_id : string;
        ob_id : string;
        ob_attributes : v_value list
    }

let create_v_value n v = { v_name = n; v_value = v }

let rec mm_string_of_value = function
   | VBoolean None -> "Undefined VBoolean"
   | VChar None -> "Undefined VChar"
   | VInt None -> "Undefined VInt"
   | VFloat None -> "Undefined VFloat"
   | VRefType None -> "Undefined VRefType"
   | VArray None -> "Undefined VArray"
   | VBoolean Some b -> string_of_bool b
   | VChar Some c -> Char.escaped c
   | VInt Some i -> string_of_int i
   | VFloat Some f -> string_of_float f
   | VRefType Some rt -> string_of_vreftype rt
   | VArray Some va -> string_of_varray va

and string_of_vreftype vr_type =
    ("Object desc "^vr_type.vref_obj_id^" of class "^vr_type.vref_class_id)

and string_of_varray va =
    let rec string_of_varray_rec va str =
        match va with
        | [] -> str
        | h :: t -> string_of_varray_rec t (str^"; "^(mm_string_of_value h))
    in
    string_of_varray_rec va ""

and string_of_v_value v_value =
    (v_value.v_name^" -> "^(mm_string_of_value v_value.v_value))

and print_v_value v_value =
    print_endline (string_of_v_value v_value)

and print_v_value_list v_value_list =
    List.iter (fun el -> print_endline ((string_of_v_value el)^"; ")) v_value_list

and print_obj_desc obj_desc =
    print_endline ("Object desc "^obj_desc.ob_id^" of class "^obj_desc.class_id);
    print_endline "Attribute values";
    List.iter print_v_value obj_desc.ob_attributes


type stack_el = 
    | Variable of v_value
    | Method of AST.astmethod


type memory = {
    class_desc_list : class_desc list;
    meth_table : AST.astmethod StringMap.t;
    
    (* Note: it would be better to build the heap as a list of v_value, because with a list of obj_desc, we cannot store anything else than objects. I won't change it due to the lack of time. *)
    heap : obj_desc list; (*TODO: we'll have to be tricky to store primary values in heap, for example by storing special obj_desc corresponding to a primary type *)
    stack : stack_el list
}

let rec find_value_in_stack name mem =
    let rec find_value_in_stack_rec st =
        match st with 
        | [] -> Pervasives.failwith ("Variable "^name^" is not in stack")
        | st -> let (v, new_st) = StackII.pop st in
                match v with
                | Method m -> find_value_in_stack_rec new_st
                | Variable vv -> 
                        if name = vv.v_name then vv
                                 else find_value_in_stack_rec new_st
    in
    find_value_in_stack_rec mem.stack

let rec update_class_desc_list cdl mem =
    {
        class_desc_list = cdl;
        meth_table = mem.meth_table;
        heap = mem.heap;
        stack = mem.stack
    }

and update_meth_table mt mem =
    {
        class_desc_list = mem.class_desc_list;
        meth_table = mt;
        heap = mem.heap;
        stack = mem.stack
    }

and update_heap h mem =
    {
        class_desc_list = mem.class_desc_list;
        meth_table = mem.meth_table;
        heap = h;
        stack = mem.stack
    }

and update_stack st mem =
    {
        class_desc_list = mem.class_desc_list;
        meth_table = mem.meth_table;
        heap = mem.heap;
        stack = st
    }

and push_to_callstack el mem =
    update_stack (StackII.push el mem.stack) mem

let rec find_class_desc_by_name class_id mem =
    let rec find_class_desc_by_name_rec class_id class_desc_list =
        match class_desc_list with
        | [] -> Pervasives.failwith ("find_class_desc_by_name: class_desc not found for class_id = "^class_id)
        | h :: t -> if h.name = class_id then h
                    else find_class_desc_by_name_rec class_id t
    in
    find_class_desc_by_name_rec class_id mem.class_desc_list

and is_in_class_desc_list class_id mem =
    let rec is_in_class_desc_list_rec class_desc_list =
        match class_desc_list with
        | [] -> false
        | h :: t -> if h.name = class_id then true
                    else is_in_class_desc_list_rec t
    in
    is_in_class_desc_list_rec mem.class_desc_list

and init_v_value name value_type init_value =
    match init_value with
    | None -> { v_name = name; v_value = empty_value value_type }
    | Some init_value -> { v_name = name; v_value = init_value }

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

(* Creates new obj_desc and adds it to memory *)
let rec new_object class_id ob_name mem =
    new_object_from_class_desc (find_class_desc_by_name class_id mem) ob_name mem

and  new_object_from_class_desc class_desc ob_name mem =
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

and obj_desc_from_class_id class_id ob_name mem =
    obj_desc_from_class_desc (find_class_desc_by_name class_id mem) ob_name mem

and obj_desc_from_class_desc class_desc ob_name mem =
    {
        class_id = class_desc.name;
        ob_id = ob_name;
        ob_attributes = create_attribute_value_list class_desc.attributes []
    }

and add_to_heap obj_desc mem =
    {
        class_desc_list = mem.class_desc_list;
        meth_table = mem.meth_table;
        heap = add_or_replace_in_heap obj_desc mem;
        stack = mem.stack
    }

and add_or_replace_in_heap obj_desc mem =
    let rec add_or_replace_rec ob_id input_heap output_heap exists =
        match input_heap with 
        | [] -> if exists = true then output_heap else output_heap @ [obj_desc]
        | h :: t -> if h.ob_id = ob_id then add_or_replace_rec ob_id t (output_heap @ [obj_desc]) true
                    else add_or_replace_rec ob_id t (output_heap @ [h]) exists
    in
    add_or_replace_rec obj_desc.ob_id mem.heap [] false


(*Returns the memory with updated value in the corresponding object descriptor*)
let rec set_attribute_value_obj_id ob_id a_name a_value mem =
    let obj_desc = find_obj_desc ob_id mem in
    let updated_obj_desc = set_attribute_value obj_desc a_name a_value in
    add_to_heap updated_obj_desc mem

and set_attribute_value obj_desc a_name a_value =
    let updated_attributes = set_value obj_desc.ob_attributes a_name a_value in
    print_endline "updated attributes OK";
    {
        class_id = obj_desc.class_id;
        ob_id = obj_desc.ob_id;
        ob_attributes = updated_attributes
    }

and set_value v_value_list name value =
    let rec insert_value il ol =
        match il with 
        | [] -> ol
        | { v_name = n; v_value = v } :: t -> if n = name then insert_value t (ol @ [create_v_value n value])
                                              else insert_value t (ol @ [create_v_value n v])
    in
    insert_value v_value_list []

and find_obj_desc ob_id mem =
    let rec find_obj_desc_rec heap =
        match heap with
        | [] -> Pervasives.failwith ("Could not find object "^ob_id^" in heap")
        | h :: t -> if h.ob_id = ob_id then h 
                    else find_obj_desc_rec t
    in
    find_obj_desc_rec mem.heap

and find_vref_type_by_name ob_id mem = 
    let obj_desc = find_obj_desc ob_id mem in
    VRefType (Some {vref_class_id = obj_desc.class_id; vref_obj_id = ob_id })

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
    print_endline "Method table (comp_name: method_content)";
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



    

let print_heap mem =
    print_endline "Heap:";
    List.iter print_obj_desc mem.heap

let string_of_stack_el el =
    match el with
    | Variable v_v -> string_of_v_value v_v
    | Method am -> ("method: "^am.mname)

let print_stack mem =
    print_endline "";
    print_endline "Stack";
    print_endline "";
    List.iter (fun el -> print_endline (string_of_stack_el el)) mem.stack;
    print_endline "";
    print_endline "End of stack";
    print_endline ""


let print_memory mem = 
    print_endline "";
    print_endline "Printing memory representation";
    print_endline "";
    print_method_table mem.meth_table;
    print_class_desc_list mem.class_desc_list;
    print_endline "";
    print_heap mem;
    print_endline "";
    print_endline "End of memory representation";
    print_endline ""


