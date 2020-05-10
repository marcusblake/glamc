(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)


module L = Llvm
module A = Ast
open Sast
open Exceptions

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions, structs) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "GlamC" in
  
  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.float_type  context
  (* and string_t   = L.pointer_type (L.i8_type context) ignore for now *)
  (* and none_t     = L.void_type   context *)
  in

  (* Return the LLVM type for a MicroC type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Char -> i8_t
    | _ -> raise Unimplemented
    (* | A.None  -> none_t *)
    (* | A.String -> string_t ignore for now *)
    (* | A.List(t) -> L.pointer_type (ltype_of_typ t) *)
  in