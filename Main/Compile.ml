open Parser
open TypeAST
open Compile_AST

let execute lexbuf verbose = 
  try 
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "********************* Successfull parsing *********************";
    if verbose then AST.print_program ast ;
    let type_AST = TypeAST.typing ast [] in
    print_endline "********************* Successfull typing *********************";
    let memory = compile_ast ast in
    print_endline "********************* Successful class compilation *********************";

    let memory = Memory_Model.new_object "Animal" "animal" memory in
    let memory = Memory_Model.new_object "Cat" "mrtinkle" memory in
    let memory = Memory_Model.new_object "Cat" "sirpounce" memory in


    Memory_Model.print_memory memory;

    let memory = Eval.eval_program memory in
    if verbose then AST.print_program ast 
  with 
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
