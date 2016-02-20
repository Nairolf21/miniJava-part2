type error_type = 
    | Asttype_not_found of string
    | Class_desc_not_found of string

exception Error of error_type

let report_error = function
    | Asttype_not_found s ->
            print_string "Asttype with id: ";
            print_string s
            print_endline " not found in the AST"
    | Asttype_not_found s ->
            print_string "Class_desc with id: ";
            print_string s
            print_endline " not found in the memory"
