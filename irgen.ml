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
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Char -> i8_t
    | _ -> raise Unimplemented
    (* | A.None  -> none_t *)
    (* | A.String -> string_t ignore for now *)
    (* | A.List(t) -> L.pointer_type (ltype_of_typ t) *)
  in

  let add_identifier map (ty, str) = 
    StringMap.add str ty map
  in

  let globalvars = List.fold_left add_identifier StringMap.empty globals in

  let add_scope table = StringMap.empty :: table in

  let build_function_body builder = 

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in
    
    let symbol_table = [globalvars] in

    (* TODO: IMPLEMENT BUILD_EXPR -> Needs to return llvalue for expression *)
    let build_expr table builder = function
      | _ -> raise Unimplemented

    let rec build_stmt_list table builder = function
      | [] -> builder
      | stmt :: tail -> 
        let (builder, table) = check_stmt table builder stmt in
          check_stmt_list table builder tail
    and build_stmt table builder = function
      | SBlock lst -> 
          let new_table = add_scope table in
            (build_stmt_list new_table builder lst, table)
      | SExpr sexpr -> ignore(build_expr table builder sexpr); (builder, table)
      (* TODO: NEED TO ADD TO SYMBOL TABLE *)
      | SExplicit ((ty, name), sexpr) ->
        let e' = build_expr table builder sexpr in
        let var = L.build_alloca (ltype_of_typ ty) name builder in
          ignore (L.build_store e' var builder);
          (builder, table)
      (* TODO: NEED TO ADD TO SYMBOL TABLE *)
      | SDefine (name, sexpr) -> 
        let e' = build_expr table builder sexpr in
        let var = L.build_alloca (ltype_of_typ (fst sepxr)) name builder in
          ignore (L.build_store e' var builder);
          (builder, table)

        
        (* This won't support nested if statements. Will need to figure out a way to support nested if statements.
            Not terribly important so we can try to fix it later *)
      | SIf (sexpr, stmt) -> 
        (*
          EXPR
          bz label_end, expr
        if_block
          Code
          br label if_end
        if_end:
        *)
        let llvalue = build_expr table builder sexpr in
        (* Need to add function declarations to a map *)
        let if_block = L.append_block context "if_block" current_function in
        ignore(build_stmt table (L.builder_at_end context if_block) builder stmt);
        let if_end = L.append_block context "if_end" current_function in
        let build_break = L.build_br if_end in
        add_terminator (L.builder_at_end context if_block) build_break;

        ignore(L.build_cond_br llvalue if_block if_end builder);
        (L.builder_at_end context if_end, table)

      | SIfElse (sexpr, stmt1, stmt2) -> 
        (*
          EXPR
          bz else_block, expr
        if_block:
          Code
          br label if_end
        else_block:
          code
          br labael if_end 
        if_end:
        *)
        let llvalue = build_expr table builder sepxr in
        let if_block = L.append_block context "if_block" current_function in
        ignore (build_stmt table (L.builder_at_end context if_block) builder stmt1);
        let else_block = L.append_block context "else_block" current_function in
        ignore (build_stmt table (L.builder_at_end context else_block) builder stmt2);
        let if_end  = L.append_block context "if_end" current_function in
        let build_break = L.build_br if_end in
        add_terminator (L.builder_at_end if_block) build_break;
        add_terminator (L.builder_at_end context else_block) build_break;

        ignore (L.build_cond_br llvalue if_block else_block builder);
        (L.builder_at_end context if_end, table)
      | SIterate (var, sexpr, stmt) -> raise Unimplemented (* ignore for now *)
      | SWhile (sexpr, stmt) -> 
        (*
        br label %while
        while:
          bz while_end expr
        while_body:
          Code
          br label %while
        while_end:

        *)
        let while_begin = L.append_block context "while" current_function in
        let build_br = L.build_br while_begin in (* partial function *)
        ignore (build_br builder);
        let llvalue = build_expr table builder sexpr in
          
        
      | SReturn (sexpr) -> ignore (L.build_ret (build_expr table builder sexpr) builder); (builder, table)
        


