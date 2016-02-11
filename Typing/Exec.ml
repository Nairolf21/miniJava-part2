open AST
open Type

type obj_desc = 
    {
        name : string;
        attributes : string list
    }

let print_obj_desc od = 
    print_endline ("name: "^od.name);
    print_endline "Attrbutes:";
    List.iter (fun el -> print_endline el) od.attributes

let rec print_obj_desc_list = function
    | [] -> print_endline "end of object descriptors"
    | h :: t -> print_obj_desc h;
                print_obj_desc_list t

let create_obj_desc astclass id = 
    let namelist = List.map (fun attr -> attr.aname) astclass.cattributes in
    { name = id; attributes = namelist }

let create_obj_desc_list ast = 
    match ast.type_list with
    | [] -> []
    | h :: t -> match h.info with
                | Inter -> []
                | Class c -> [create_obj_desc c h.id]


