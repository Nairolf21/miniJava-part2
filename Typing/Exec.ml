open AST
open Type

(* TODO: Be more specific with the attributes : add modifiers and type ?
 * Or is it to be made by the typing team ?*)
type obj_desc = 
    {
        name : string;
        attributes : string list
    }

(* Creates an object descriptor based on an AST.astclass. Should be called from 
 * create_obj_desc_list *)
let create_obj_desc astclass id = 
    let namelist = List.map (fun attr -> attr.aname) astclass.cattributes in
    { name = id; attributes = namelist }

(* Creates a list of object descriptors from an AST.t.
 * TODO: Read the package info. For the moment, it only construct the list for
 * a single file. (take inspiration from AST.print_program for that) *)
let create_obj_desc_list ast = 
    match ast.type_list with
    | [] -> []
    | h :: t -> match h.info with
                | Inter -> []
                | Class c -> [create_obj_desc c h.id]


(*Printing functions *)

let print_obj_desc od = 
    print_endline ("name: "^od.name);
    print_endline "Attrbutes:";
    List.iter (fun el -> print_endline el) od.attributes

let rec print_obj_desc_list = function
    | [] -> print_endline "end of object descriptors"
    | h :: t -> print_obj_desc h;
                print_obj_desc_list t

