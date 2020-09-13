open Sys
open Printf

type action = Tokens | Ast | Sast | LLVM_IR | Exec

let () =
  let action = ref Exec in
  let output = ref "a.out" in
  let set_action a () = action := a in
  let set_output name = output := name in
  let speclist =
    [
      ("-t", Arg.Unit (set_action Tokens), "Print the generated tokens");
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-s", Arg.Unit (set_action Sast), "Print the SAST");
      ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
      ("-o", Arg.String set_output, "Output file");
    ]
  in
  let usage_msg =
    "usage: ./glamc.native [-t|-a|-s|-l] [file.mc] -o <executable>"
  in
  let file = ref "" in
  Arg.parse speclist (fun filename -> file := filename) usage_msg;

  let ast = Parse_driver.parse_program !file in
  match !action with
  | Tokens -> print_string (Scanner.string_of_tokens !file)
  | Ast -> print_string (Print_ast.string_of_program ast)
  | _ -> (
      let sast = Semant.check ast in
      let llvm_module = Llvm.string_of_llmodule (Irgen.translate sast) in
      match !action with
      | Tokens -> ()
      | Ast -> ()
      | Sast -> ()
      | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast))
      | Exec ->
          let out = open_out "llvm.out" in
          fprintf out "%s\n" llvm_module;
          close_out out;
          if command "llc -relocation-model=pic llvm.out" != 0 then
            raise (Failure "llc: non-zero exit code")
          else if command ("g++ llvm.out.s -L./ -lglamc -o " ^ !output) != 0
          then raise (Failure "g++: non-zero exit code")
          else () )
