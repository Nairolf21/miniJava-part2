open Parser
open TypeAST

let execute lexbuf verbose = 
  try 
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    let astType = TypeAST.typing ast in
    print_endline "successfull typing";
    if verbose then AST.print_program ast;
  with 
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
