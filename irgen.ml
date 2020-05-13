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
(* Ignore structs for now *)
let translate (globals, functions, _) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "GlamC" in
  
  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.float_type  context
  and void_t     = L.void_type   context
  and string_t   = L.named_struct_type context "string"
  (* and none_t     = L.void_type   context *)
  in

  (* 
  struct String {
    int length;
    char *elements;
  }
  *)
  let _ =
    L.struct_set_body string_t [| i32_t ; L.pointer_type i8_t |] false in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Char -> i8_t
    | A.String -> string_t
    | _ -> raise Unimplemented
    (* | A.None  -> none_t *)
    (* | A.String -> string_t ignore for now *)
    (* | A.List(t) -> L.pointer_type (ltype_of_typ t) *)
  in

  (* Return an initialized value of a given type *)
  let initialized_value = function 
    | A.Int -> L.const_int i32_t 0
    | A.Bool -> L.const_int i1_t 0 (* Initialize to false *)
    | A.Float -> L.const_float float_t 0.0
    | A.Char -> L.const_int i8_t 0
    | _ -> raise Unimplemented
  in


  let add_identifier map (ty, str) = 
    let var = L.define_global str (initialized_value ty) the_module in 
      StringMap.add str (var, ty) map
  in

  let globalvars : (L.llvalue * Ast.ty) StringMap.t = List.fold_left add_identifier StringMap.empty globals in

  let add_scope table = StringMap.empty :: table in

  let add_to_current_scope table name llval ty =
    List.mapi (fun idx map -> if idx = 0 then StringMap.add name (llval, ty) map else map) table
  in

  (* BEGIN: Definitions for String library functions *)
  let initString_t = L.function_type void_t [| L.pointer_type string_t; L.pointer_type i8_t |] in
  let strLength_t = L.function_type i32_t [| string_t |] in
  let getChar_t = L.function_type i8_t [| string_t; i32_t |] in
  let prints_t = L.function_type void_t [| string_t |] in

  let initString = L.declare_function "initString" initString_t the_module in
  let strLength = L.declare_function "lenstr" strLength_t the_module in
  let getChar = L.declare_function "getChar" getChar_t the_module in
  let prints = L.declare_function "prints" prints_t the_module in
  (* END: Definitions for String library functions *)
  

  (* print boolean *)
  let printb = L.declare_function "printb" (L.function_type void_t [| i1_t |]) the_module in


  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Function that takes in the name of a variable, and symbol_table. It returns the LLVM representation of the variable *)
  let rec lookup_identifier name = function
      | [] -> raise (UnrecognizedIdentifier "TODO: ERROR MESSAGE")
      | current_scope :: tl -> 
        try
          StringMap.find name current_scope
        with Not_found ->
          lookup_identifier name tl
    in

  let function_decls : (L.llvalue * sfunc_def) StringMap.t = 
    let function_decl map fdecl = 
      let name = fdecl.sfunc_name 
      and formal_types = 
        Array.of_list (List.map (fun (ty, _) -> ltype_of_typ ty) fdecl.sparameters) in
      let llvalue = L.function_type (ltype_of_typ fdecl.sreturn_type) formal_types in
      StringMap.add name (L.define_function name llvalue the_module, fdecl) map in
      List.fold_left function_decl StringMap.empty functions in

  let build_function_body fdecl = 
    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in
    
    let (current_function, _) = StringMap.find fdecl.sfunc_name function_decls in 
    
    let builder : L.llbuilder = L.builder_at_end context (L.entry_block current_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt_int" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt_float" builder in
    let char_format_str = L.build_global_stringptr "\'%c\'\n" "fmt_char" builder in

    let formals : (L.llvalue * Ast.ty) StringMap.t =
      let add_formal map (ty, name) param =
        L.set_value_name name param;
        let local = L.build_alloca (ltype_of_typ ty) name builder in
        ignore (L.build_store param local builder);
        StringMap.add name (local, ty) map
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sparameters (Array.to_list (L.params current_function))
    in


    let symbol_table : (L.llvalue * Ast.ty) StringMap.t list = [formals; globalvars] in

    (* TODO: IMPLEMENT BUILD_EXPR -> Needs to return llvalue for expression *)
    let rec build_expr table builder ((_, e) : sexpr) = match e with
      SIntLit i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit f -> L.const_float float_t f
      | SCharLit c -> L.const_int i8_t (int_of_char c)
      | SStringLit s -> 
          let strptr = L.build_global_stringptr s "string_const" builder in
          let temp = L.build_alloca string_t "" builder in
            ignore(L.build_call initString [|temp; strptr|] "" builder);
            L.build_load temp "temp_string" builder;
      | SId s -> L.build_load (fst (lookup_identifier s table)) s builder
      | SAssign (s, e) -> let e' = build_expr table builder e in
        ignore(L.build_store e' (fst (lookup_identifier s table)) builder); e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr table builder e1
        and e2' = build_expr table builder e2 in
        (match op with
        A.Add       -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.Mod     -> L.build_srem
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
        (* Float operations *)
        (*   A.Add	-> L.build_fadd
					| A.Sub		-> L.build_fsub
					| A.Mult	-> L.build_fmul
					| A.Div		-> L.build_fdiv
					| A.Mod		-> L.build_frem
					| A.Equal	-> L.build_fcmp L.Fcmp.Oeq
					| A.Neq		-> L.build_fcmp L.Fcmp.One
					| A.Less	-> L.build_fcmp L.Fcmp.Olt
					| A.Leq		-> L.build_fcmp L.Fcmp.Ole
					| A.Greater	-> L.build_fcmp L.Fcmp.Ogt
          | A.Geq		-> L.build_fcmp L.Fcmp.Oge 
          | _         -> raise (Foo "Invalid Float Operator")
					) e1' e2' "tmp" builder in *)
      | SCall (f, args) ->
        (try (* try to see if it's a built in function first *)
          match args with
          [e] -> check_built_in f e table builder
          | _ -> raise Not_found
        with Not_found ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let llargs = List.rev (List.map (build_expr table builder) (List.rev args)) in
          let result = f ^ "_result" in
          L.build_call fdef (Array.of_list llargs) result builder)
      | SSeqAccess(name, e) -> 
        let ellv = build_expr table builder e in
        let (strllv, _) = lookup_identifier name table in
        let value = L.build_load strllv "" builder in
        L.build_call getChar [| value; ellv |] "idx" builder 
      (*| StructCall -> raise Unimplemented
      | StructAccess -> raise Unimplemented
      | StructAssign -> raise Unimplemented *)
      | _ -> raise Unimplemented
    and check_built_in name e table builder =
      if name = "printi" then
        L.build_call printf_func [| int_format_str ; (build_expr table builder e) |] "printf" builder
      else if name = "printfl" then 
        L.build_call printf_func [| float_format_str ; (build_expr table builder e) |] "printf" builder
      else if name = "printc" then 
        L.build_call printf_func [| char_format_str ; (build_expr table builder e) |] "printf" builder
      else if name = "printb" then
        let v = build_expr table builder e in
        L.build_call printb [| v |] "" builder
      else if name = "prints" then 
        let v = build_expr table builder e in
        L.build_call prints [| v |] "" builder
      else if name = "lenstr" then
        let v = build_expr table builder e in
        L.build_call strLength [| v |] "length" builder
      else raise Not_found
    in

    let rec build_stmt_list table builder = function
      | [] -> builder
      | stmt :: tail -> 
        let (builder, table) = build_stmt table builder stmt in
          build_stmt_list table builder tail
    and build_stmt table builder = function
      | SBlock lst -> 
          let new_table = add_scope table in
            (build_stmt_list new_table builder lst, table)
      | SExpr sexpr -> ignore(build_expr table builder sexpr); (builder, table)
      | SExplicit ((ty, name), sexpr) ->
        let e' = build_expr table builder sexpr in
        let var = L.build_alloca (ltype_of_typ ty) name builder in
        let table = add_to_current_scope table name var ty in
          ignore (L.build_store e' var builder);
          (builder, table)
      | SDefine (name, sexpr) -> 
        let e' = build_expr table builder sexpr in
        let var = L.build_alloca (ltype_of_typ (fst sexpr)) name builder in
        let table = add_to_current_scope table name var (fst sexpr) in
          ignore (L.build_store e' var builder);
          (builder, table)
        (* This won't support nested if statements. Will need to figure out a way to support nested if statements.
            Not terribly important so we can try to fix it later *)
      | SIf (sexpr, stmt) -> 
        (*
          EXPR
          bz label_end, expr
        if_block:
          Code
          br label if_end
        if_end:
        *)
        let llvalue = build_expr table builder sexpr in
        (* Need to add function declarations to a map *)
        let if_block = L.append_block context "if_block" current_function in
        ignore(build_stmt table (L.builder_at_end context if_block) stmt);
        let if_end = L.append_block context "if_end" current_function in
        let build_break = L.build_br if_end in
        add_terminal (L.builder_at_end context if_block) build_break;

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
        let llvalue = build_expr table builder sexpr in
        let if_block = L.append_block context "if_block" current_function in
        ignore (build_stmt table (L.builder_at_end context if_block) stmt1);
        let else_block = L.append_block context "else_block" current_function in
        ignore (build_stmt table (L.builder_at_end context else_block) stmt2);
        let if_end  = L.append_block context "if_end" current_function in
        let build_break = L.build_br if_end in
        add_terminal (L.builder_at_end context if_block) build_break;
        add_terminal (L.builder_at_end context else_block) build_break;

        ignore (L.build_cond_br llvalue if_block else_block builder);
        (L.builder_at_end context if_end, table)
      | SIterate (var, sexpr, stmt) -> raise Unimplemented (* ignore for now *)
      | SWhile (sexpr, stmt) -> 
        (*
          br label %while
          while:
            expr
            bz while_end expr
          while_body:
            Code
            br label %while
          while_end:
        *)
        let while_begin = L.append_block context "while" current_function in

        let start_while = L.build_br while_begin in (* partial function *)
        ignore (start_while builder);

        let while_builder = L.builder_at_end context while_begin in

        let llvalue = build_expr table while_builder sexpr in
        let while_body = L.append_block context "while_body" current_function in
        let (updated_builder, _) = build_stmt table (L.builder_at_end context while_body) stmt in
        add_terminal updated_builder start_while;

        let while_end = L.append_block context "while_end" current_function in

        ignore (L.build_cond_br llvalue while_body while_end while_builder);
        (L.builder_at_end context while_end, table)
        
      | SReturn (sexpr) -> ignore (L.build_ret (build_expr table builder sexpr) builder); (builder, table)
    in
    
    let funcbuilder = (match fdecl.sbody with
    | SBlock lst -> build_stmt_list symbol_table builder lst (* Flatten sstmt *)
    | _ -> raise Invalid)
    in

    add_terminal funcbuilder (L.build_ret (initialized_value A.Int))

  in
  List.iter build_function_body functions;
  the_module
