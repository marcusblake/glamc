let _ = 
  let input = open_in "scanner_test.gc" in
  let lexbuf = Lexing.from_channel input in
  let prog = Parser.program Scanner.token lexbuf in
  check_program prog
  (* print_endline (string_of_program prog) *)


  (*let newContext = add_to_context context func_name (Arrow (List.map fst) )*)
(* A struct has a name, data fields, and functions *)

(* 
  struct Point {
    int x;
    int y;

    func getSum() int {
      return x + y;
    }
  }
*)