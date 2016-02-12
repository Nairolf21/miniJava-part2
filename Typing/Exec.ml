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


let rec add_classes_to_memory type_list mem =
    match type_list with 
    | [] -> mem
    | h :: t -> match h.info with 
                | Inter -> mem
                | Class c -> add_classes_to_memory t (add_class_to_memory c h.id mem)

(* AST.t -> string -> memory *)
and add_class_to_memory astclass id_class mem =
    {
        meth_table =  add_methods astclass.cmethods id_class mem.meth_table;
        class_desc_list = mem.class_desc_list @ [create_class_desc astclass id_class]
    }

(* TODO: Inheritance *)
let compile_classes ast = 
    add_classes_to_memory ast.type_list { meth_table = []; class_desc_list = [] }


(*Printing functions *)

let print_class_desc cd = 
    print_endline ("name: "^cd.name);
    print_endline "Attrbutes:";
    List.iter (fun el -> print_endline el) cd.attributes;
    print_endline "Methods:";
    List.iter (fun el -> print_endline el) cd.method_names

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

