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
  and i64_t      = L.i64_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.float_type  context
  and void_t     = L.void_type   context
  and string_t   = L.named_struct_type context "string"
  and list_t     = L.named_struct_type context "list"
  (* and none_t     = L.void_type   context *)
  in

  (* 
  struct String {
    int length;
    char *elements;
  }

  struct List {
		int length;
		int element_size;
		char *list;
	};
  *)
  let _ =
    ignore(L.struct_set_body string_t [| i32_t ; L.pointer_type i8_t |] false);
    L.struct_set_body list_t   [| i32_t ; i32_t ; L.pointer_type i8_t |] false in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Char -> i8_t
    | A.String -> string_t
    | A.List _ -> list_t
    | _ -> raise Unimplemented
    (* | A.None  -> none_t *)
    (* | A.String -> string_t ignore for now *)
    (* | A.List(t) -> L.pointer_type (ltype_of_typ t) *)
  in


  (* BEGIN: Definitions for String library functions *)
  let initString_t = L.function_type void_t [| L.pointer_type string_t; L.pointer_type i8_t |] in
  let strLength_t = L.function_type i32_t [| L.pointer_type string_t |] in
  let getChar_t = L.function_type i8_t [| L.pointer_type string_t; i32_t |] in
  let prints_t = L.function_type void_t [| L.pointer_type string_t |] in

  let initString = L.declare_function "initString" initString_t the_module in
  let strLength = L.declare_function "lenstr" strLength_t the_module in
  let getChar = L.declare_function "getChar" getChar_t the_module in
  let prints = L.declare_function "prints" prints_t the_module in
  (* END: Definitions for String library functions *)

  
  (* BEGIN: Definition for List library function *)
  let initList_t = L.function_type void_t [| L.pointer_type list_t; i64_t; i32_t; L.pointer_type i8_t |] in
  let getEl_t = L.function_type void_t [| L.pointer_type list_t; i32_t; L.pointer_type i8_t |] in 
  let addEl_t = L.function_type void_t [| L.pointer_type list_t; L.pointer_type i8_t |] in
  let setEl_t = L.function_type void_t [| L.pointer_type list_t; i32_t; L.pointer_type i8_t |] in
  let popEl_t = L.function_type void_t [| L.pointer_type list_t |] in 
  let lenlist_t = L.function_type i32_t [| L.pointer_type list_t |] in

  let initList = L.declare_function "initList" initList_t the_module in
  let getElement= L.declare_function "getElement" getEl_t  the_module in
  let addElement = L.declare_function "addElement" addEl_t the_module in
  let setElement = L.declare_function "setElement" setEl_t  the_module in
  let popElement= L.declare_function "popElement" popEl_t  the_module in
  let lenlist = L.declare_function "lenlist" lenlist_t the_module in
  (* END: Definition for List library function *)
  

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
      let get_type ty = 
        let llty = ltype_of_typ ty in
        match ty with
        | String | List _ -> L.pointer_type llty
        | _ -> llty
      in
      let name = fdecl.sfunc_name 
      and formal_types = 
        Array.of_list (List.map (fun (ty, _) -> get_type ty) fdecl.sparameters) in
      let llvalue = L.function_type (get_type fdecl.sreturn_type) formal_types in
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
    let empty_str = L.build_global_stringptr "" "empty_str" builder in

    (* Return an initialized value of a given type *)
    let initialized_value = function 
        | A.Int -> L.const_int i32_t 0
        | A.Bool -> L.const_int i1_t 0 (* Initialize to false *)
        | A.Float -> L.const_float float_t 0.0
        | A.Char -> L.const_int i8_t 0
        | A.String ->  L.const_named_struct string_t [| L.const_int i32_t 0; L.const_pointer_null i8_t |]
        | A.List _ -> L.const_named_struct list_t [| L.const_int i32_t 0; L.const_int i32_t 0; L.const_pointer_null i8_t |]
        | _ -> raise Unimplemented
    in

    let add_identifier map (ty, str) = 
      let var = L.define_global str (initialized_value ty) the_module in 
      let _ = match ty with
      | A.String -> ignore(L.build_call initString [|var; empty_str|] "" builder)
      | A.List el_ty -> 
        let ptr = L.build_load (L.build_struct_gep var 2 "" builder) "" builder in
        ignore(L.build_call initList [| var; L.size_of (ltype_of_typ el_ty); L.const_int i32_t 0; ptr |] "" builder)
      | _ -> ()
      in
      StringMap.add str (var, ty) map
    in
  
    let globalvars : (L.llvalue * Ast.ty) StringMap.t = List.fold_left add_identifier StringMap.empty globals in
  
    let add_scope table = StringMap.empty :: table in
  
    let add_to_current_scope table name llval ty =
      List.mapi (fun idx map -> if idx = 0 then StringMap.add name (llval, ty) map else map) table
    in
  

    let formals : (L.llvalue * Ast.ty) StringMap.t =
      let add_formal map (ty, name) param =
        L.set_value_name name param;
        let local = L.build_alloca (ltype_of_typ ty) name builder in
        ignore (match ty with
        | String | List _ -> let copy = L.build_load param "" builder in L.build_store copy local builder;
        | _ -> L.build_store param local builder;);
        StringMap.add name (local, ty) map
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sparameters (Array.to_list (L.params current_function))
    in


    let symbol_table : (L.llvalue * Ast.ty) StringMap.t list = [formals; globalvars] in

    (* TODO: IMPLEMENT BUILD_EXPR -> Needs to return llvalue for expression *)
    let rec build_expr table builder ((type_, e) : sexpr) = match e with
      SIntLit i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit f -> L.const_float float_t f
      | SCharLit c -> L.const_int i8_t (int_of_char c)
      | SStringLit s -> 
          let strptr = L.build_global_stringptr s "string_const" builder in
          let strct = L.const_named_struct string_t [| L.const_int i32_t 0; strptr |] in 
          let global = L.define_global "tmp_string" strct the_module in
            ignore(L.build_call initString [|global; strptr|] "" builder); global
      | SSeq lst -> 
          let ty = (match type_ with A.List ty -> ty | _ -> raise Invalid) in
          let len = List.length lst in (* Get length of list *)
          let llvlen = L.const_int i32_t len in (* llvm const of length *)
          let llvty = (ltype_of_typ ty) in (* type of list elements *)
          let llvlst = List.map (build_expr table builder) lst in
          let strct = L.const_named_struct list_t [| L.const_int i32_t 0; L.const_int i32_t 0; L.const_pointer_null i8_t |] in 
          let global = L.define_global "lst" strct the_module in
          let add_to_lst a = let generic = L.build_bitcast a (L.pointer_type i8_t) "" builder in
            L.build_call addElement [| global; generic |] "" builder
          in
          let _ = match ty with 
              A.String | A.List _ -> 
                let ptr = L.build_load (L.build_struct_gep global 2 "" builder) "" builder in
                ignore(L.build_call initList [| global; L.size_of llvty; L.const_int i32_t 0; ptr |] "" builder);
                ignore(List.map add_to_lst llvlst)
              | _ -> let llvarray = Array.of_list (List.map (build_expr table builder) lst) in (* array of llvm values *)
                    let buffer = L.define_global "tmparr" (L.const_array llvty llvarray) the_module in (* Set as global variable *)
                     let ptr = L.build_bitcast buffer (L.pointer_type i8_t) "buff_ptr" builder in
                    ignore(L.build_call initList [| global; L.size_of llvty; llvlen; ptr |] "" builder)
            in
            global
      | SId s -> 
            let (llv, ty) = lookup_identifier s table in
            (match ty with A.String | A.List _ -> llv | _ -> L.build_load llv s builder)
      | SAssign (s, e) -> let e' = build_expr table builder e in
        ignore(L.build_store e' (fst (lookup_identifier s table)) builder); e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr table builder e1
        and e2' = build_expr table builder e2 in
        (match op with
        A.Add -> L.build_add
        | A.Sub -> L.build_sub
        | A.Mult -> L.build_mul
        | A.Div -> L.build_sdiv
        | A.And -> L.build_and
        | A.Or -> L.build_or
        | A.Equal -> L.build_icmp L.Icmp.Eq
        | A.Neq -> L.build_icmp L.Icmp.Ne
        | A.Less -> L.build_icmp L.Icmp.Slt
        | A.Leq -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
      | SCall (f, args) ->
        (try (* try to see if it's a built in function first *)
          check_built_in f args table builder
        with Not_found ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let llargs = List.rev (List.map (build_expr table builder) (List.rev args)) in
          let result = f ^ "_result" in
          L.build_call fdef (Array.of_list llargs) result builder)
      | SSeqAccess(name, e) -> 
        let index_llval = build_expr table builder e in
        let (llval, iden_ty) = lookup_identifier name table in
        (match iden_ty with
        | A.String -> L.build_call getChar [| llval; index_llval |] "idx" builder
        | A.List ty -> let temp = L.build_alloca (ltype_of_typ ty) "tmp_store" builder in
            let generic_ptr = L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder in
              ignore (L.build_call getElement [| llval ; index_llval ; generic_ptr |] "" builder);
              (match ty with
              | String | List _ -> temp
              | _ -> L.build_load temp "list_item" builder)
        | _ -> raise Invalid)
      (*| StructCall -> raise Unimplemented
      | StructAccess -> raise Unimplemented
      | StructAssign -> raise Unimplemented *)
      | _ -> raise Unimplemented
    and check_built_in name e table builder =
      let args = List.map (build_expr table builder) e in
      if name = "printi" then
        L.build_call printf_func (Array.of_list ([int_format_str] @ args)) "printf" builder
      else if name = "printfl" then 
        L.build_call printf_func (Array.of_list ([float_format_str] @ args)) "printf" builder
      else if name = "printc" then 
        L.build_call printf_func (Array.of_list ([char_format_str] @ args)) "printf" builder
      else if name = "printb" then
        L.build_call printb (Array.of_list args) "" builder
      else if name = "prints" then 
        L.build_call prints (Array.of_list args) "" builder
      else if name = "lenstr" then
        L.build_call strLength (Array.of_list args) "length" builder
      else if name = "append" then
        let el = List.nth args 1 in
        let (ty, _) = List.nth e 1 in
        let lst = List.hd args in
        let temp = L.build_alloca (ltype_of_typ ty) "" builder in
            ignore (match ty with
            | String | List _ -> let v = L.build_load el "" builder in L.build_store v temp builder
            | _ -> L.build_store el temp builder);
        let generic = L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder in
          L.build_call addElement [| lst; generic |] "" builder
      else if name = "lenlist"  then
        L.build_call lenlist (Array.of_list args) "" builder
      else if  name = "pop" then
        L.build_call popElement (Array.of_list args) "" builder
      else if name = "put" then
        let el = List.nth args 2 in
        let (ty, _) = List.nth e 2 in
        let idx = List.nth args 1 in
        let lst = List.hd args in
        let temp = L.build_alloca (ltype_of_typ ty) "" builder in
            ignore (match ty with
            | String | List _ -> let v = L.build_load el "" builder in L.build_store v temp builder
            | _ -> L.build_store el temp builder);
        let generic = L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder in
          L.build_call setElement [| lst; idx; generic |] "" builder
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
        let _ = match ty with
              | String | List _ -> let v = L.build_load e' "temp" builder in L.build_store v var builder
              | _ -> L.build_store e' var builder
        in
        (builder, table)
      | SDeclare(ty, name) -> 
        let var = L.build_alloca (ltype_of_typ ty) name builder in
        let _ = match ty with
              | String -> L.build_call initString [|var; empty_str|] "" builder
              | List el_ty -> 
                let ptr = L.build_load (L.build_struct_gep var 2 "" builder) "" builder in
                L.build_call initList [| var; L.size_of (ltype_of_typ el_ty); L.const_int i32_t 0; ptr |] "" builder
              | _ -> L.build_store (initialized_value ty) var builder
        in
        let table = add_to_current_scope table name var ty in
        (builder, table)
      | SDefine (name, sexpr) -> 
        let e' = build_expr table builder sexpr in
        let ty = fst sexpr in
        let var = L.build_alloca (ltype_of_typ ty) name builder in
        let table = add_to_current_scope table name var ty in
        let _ = match ty with
              | String | List _ -> let v = L.build_load e' "temp" builder in L.build_store v var builder
              | _ -> L.build_store e' var builder
        in
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
        
      | SReturn (sexpr) -> 
        let ty = fst sexpr in
        let e = build_expr table builder sexpr in
        let return_val = (match ty with
        | A.List _ | A.String -> 
          let mem = L.build_malloc (ltype_of_typ ty) "return" builder in (* need to malloc so that object one roll back in stack*)
          ignore(L.build_store (L.build_load e "" builder) mem builder); mem
        | _ -> e)
        in
        ignore(L.build_ret return_val builder); (builder, table)
    in
    
    let funcbuilder = (match fdecl.sbody with
    | SBlock lst -> build_stmt_list symbol_table builder lst (* Flatten sstmt *)
    | _ -> raise Invalid)
    in

    add_terminal funcbuilder (L.build_ret (initialized_value A.Int))

  in
  List.iter build_function_body functions;
  the_module
