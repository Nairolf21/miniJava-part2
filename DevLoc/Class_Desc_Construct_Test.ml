module StringMap = Map.Make(String)

let sm_list_keys map =
    let rec list_keys_rec assoc_list key_list =
        match assoc_list with
        | [] -> []
        | [(k, _)] -> key_list @ [k]
        | (k, _) :: t -> key_list @ (list_keys_rec t [k]) 
    in
    list_keys_rec (StringMap.bindings map) [];;

let print_el k v =
    print_endline (k^" -> "^v);;


let full_mname mname classname = classname ^ "_" ^ mname;;

let construct_meth_table_key method_name parent_mmap child_id child_mlist =
    if ListII.is_in_list method_name child_mlist then full_mname method_name child_id 
    else StringMap.find method_name parent_mmap;;



let add_parent_methods parent_mmap child_mlist parent_id child_id =
    let rec add_parent_methods_rec parent_mlist child_mlist child_mmap =
        print_endline "parent_mlist";
        List.iter (fun el -> print_string (el^";")) parent_mlist;
        print_endline "";
        print_endline "child_mlist";
        List.iter (fun el -> print_string (el^";")) child_mlist;
        print_endline "";
        match parent_mlist with
        | [] -> child_mmap
        | h :: t -> 
                let new_child_list = ListII.remove h child_mlist in
                print_endline "new_child_list: OK";
                let method_table_key = construct_meth_table_key h parent_mmap child_id child_mlist in
                print_endline "method_table_key: OK";
                let updated_child_mmap = StringMap.add h method_table_key child_mmap in
                print_endline "updated_child_mmap: OK";
                add_parent_methods_rec t new_child_list updated_child_mmap 
    in
    add_parent_methods_rec (sm_list_keys parent_mmap) child_mlist StringMap.empty
;;




let animal_mmap = StringMap.empty;;
let animal_mmap = StringMap.add "toString" "Object_toString" animal_mmap;;
let animal_mmap = StringMap.add "walk" "Animal_walk" animal_mmap;;
let animal_mmap = StringMap.add "run" "Animal_run" animal_mmap;;

let cat_mlist = ["walk"; "run"; "meow"];;

print_endline "before";;
print_endline "";;
print_endline "animal_mmap";;
sm_list_keys animal_mmap;;
print_endline "";;
let cat_mmap = add_parent_methods animal_mmap cat_mlist "Animal" "Cat";;
print_endline "cat_mmap after adding parent methods";;
StringMap.iter print_el cat_mmap;;
