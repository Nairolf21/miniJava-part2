open Parser
open Compile_AST

let execute lexbuf verbose = 
  try 
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    let memory = compile_ast ast in
    print_endline "successful class compilation";
    Memory_Model.print_memory memory;
    if verbose then AST.print_program ast 
  with 
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
