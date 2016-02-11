open AST
open Type

(* TODO: Be more specific with the attributes : add modifiers and type ?
 * Or is it to be made by the typing team ?*)
type class_desc = 
    {
        name : string;
        attributes : string list;
        method_names : string list
    }

type compiled_method = 
    {
        comp_name : string;
        meth : astmethod
    }


type memory =
    {
        meth_table : compiled_method list;
        class_desc_list : class_desc list
    }

(* Creates an object descriptor based on an AST.astclass. Should be called from 
 * create_class_desc_list. *)
(* astclass -> string -> class_desc *)
let create_class_desc astclass id = 
    let namelist = List.map (fun attr -> attr.aname) astclass.cattributes in
    let methodlist= List.map (fun meth -> meth.mname) astclass.cmethods in
    { name = id; attributes = namelist; method_names = methodlist }

(* AST.astclass -> string -> compiled_method list *)
let rec add_methods astclass id_class meth_table =
    match astclass.cmethods with 
    | [] -> meth_table
    | h :: t -> add_methods astclass id_class (meth_table @ [create_comp_method id_class h])

and create_comp_method id_class astmethod =
    {
        comp_name = id_class^"_"^astmethod.mname;
        meth = astmethod
    }

let rec add_classes_to_memory type_list mem =
    match type_list with 
    | [] -> mem
    | h :: t -> match h.info with 
                | Inter -> mem
                | Class c -> add_classes_to_memory t (add_class_to_memory c h.id mem)

(* AST.t -> string -> memory *)
and add_class_to_memory astclass id_class mem =
    {
        meth_table =  add_methods astclass id_class mem.meth_table;
        class_desc_list = mem.class_desc_list @ [create_class_desc astclass id_class]
    }

let compile_classes ast = 
    add_classes_to_memory ast.type_list { meth_table = []; class_desc_list = [] }

(* Creates a list of object descriptors from an AST.t.
 * TODO: Read the package info. For the moment, it only construct the list for
 * a single file. (take inspiration from AST.print_program for that) *)
(* AST.t -> class_desc list *)
let create_class_desc_list ast = 
    match ast.type_list with
    | [] -> []
    | h :: t -> match h.info with
                | Inter -> []
                | Class c -> [create_class_desc c h.id]


(*Printing functions *)

let print_class_desc cd = 
    print_endline ("name: "^cd.name);
    print_endline "Attrbutes:";
    List.iter (fun el -> print_endline el) cd.attributes;
    print_endline "Methods:";
    List.iter (fun el -> print_endline el) cd.method_names

let rec print_class_desc_list = function
    | [] -> print_endline "end of object descriptors"
    | h :: t -> print_class_desc h;
                print_class_desc_list t

