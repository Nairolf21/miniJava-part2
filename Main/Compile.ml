open Parser
open TypeAST
open Compile_AST

let execute lexbuf verbose = 
  try 
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast ;
    let type_AST = TypeAST.typing ast [] in
    print_endline "successfull typing";
    let memory = compile_ast ast in
    print_endline "successful class compilation";
    Memory_Model.print_memory memory;

  with 
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
