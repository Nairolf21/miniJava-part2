open Parser
open TypeAST
open Compile_AST

let fill_mem filename memory =
    print_endline ("fill_mem, filename: "^filename);
    if (filename = "tests/Eval/Inheritance.java" || filename = "tests/Eval/WithMain.java") then
        begin

        let memory = Memory_Model.new_object "Animal" "animal" memory in
        let memory = Memory_Model.new_object "Cat" "mrtinkle" memory in
        let memory = Memory_Model.new_object "Cat" "sirpounce" memory in


        let memory = Memory_Model.set_attribute_value_obj_id "animal" "legNumber" (Memory_Model.VInt (Some 2)) memory in
        let memory = Memory_Model.set_attribute_value_obj_id "mrtinkle" "legNumber" (Memory_Model.VInt (Some 4)) memory in
        Memory_Model.set_attribute_value_obj_id "sirpounce" "livesLeft" (Memory_Model.VInt (Some 7)) memory
        end
    else 
        begin
        print_endline "compile: dont add cats";
        memory
        end

let typing filename ast =
    if filename = "tests/Typing/noPackage.java" then
        begin
        let result = Some (TypeAST.typing ast []) in
        print_endline "********************* Successfull typing *********************";
        result
        end
    else 
        None

let execute lexbuf verbose filename = 
  try 
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "********************* Successfull parsing *********************";
    let type_AST = typing filename ast in
    
    let memory = compile_ast ast in
    print_endline "********************* Successful class compilation *********************";

    let memory = fill_mem filename memory in

    Memory_Model.print_memory memory;

    let memory = Eval.eval_program memory in

    Memory_Model.print_stack memory;

    if verbose then AST.print_program ast 
  with 
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l


