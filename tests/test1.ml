open Ast

let _ = 
  let input = open_in "scanner_test.gc" in
  let lexbuf = Lexing.from_channel input in
  let prog = Parser.program Scanner.token lexbuf in
  	ignore (print_endline (string_of_program prog));
		Semant.check prog
