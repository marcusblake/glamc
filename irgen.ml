(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

open Sast
open Exceptions
open Helper

module L = Llvm
module A = Ast
module St = Symbol_table
module StringMap = Map.Make(String)
module Set = Set.Make(String)

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
    L.struct_set_body list_t   [| i32_t ; i32_t; i32_t ; L.pointer_type (L.pointer_type i8_t) |] false 
  in

  (* Return the LLVM type for a MicroC type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Char -> i8_t
    | A.String -> string_t
    | A.List _ -> list_t
    | A.Function (params, return) -> 
      let get_type ty =
        let lty = ltype_of_typ ty in
        if is_iterable ty then (
          L.pointer_type lty
        ) else (
          lty
        )
      in
      let parameters = Array.of_list (List.map get_type params) in
      let return_type = ltype_of_typ return in
      L.pointer_type (L.function_type return_type parameters)
    | A.Void -> void_t
    | _ -> raise Unimplemented
  in


  (* BEGIN: Definitions for String library functions *)
  let initString_t = L.function_type void_t [| L.pointer_type string_t; L.pointer_type i8_t |] in
  let strLength_t = L.function_type i32_t [| L.pointer_type string_t |] in
  let getChar_t = L.function_type i8_t [| L.pointer_type string_t; i32_t |] in
  let prints_t = L.function_type void_t [| L.pointer_type string_t |] in
  let concat_t = L.function_type void_t [|L.pointer_type string_t; L.pointer_type string_t; L.pointer_type string_t |] in
  let split_t = L.function_type void_t [|L.pointer_type string_t; i8_t ; L.pointer_type list_t |] in
  let compare_t = L.function_type i1_t [| L.pointer_type string_t; L.pointer_type string_t; i1_t |] in

  let initString = L.declare_function "initString" initString_t the_module in
  let strLength = L.declare_function "lenstr" strLength_t the_module in
  let getChar = L.declare_function "getChar" getChar_t the_module in
  let prints = L.declare_function "prints" prints_t the_module in
  let concat = L.declare_function "concat" concat_t the_module in
  let split = L.declare_function "split" split_t the_module in
  let compare = L.declare_function "compare_string" compare_t the_module in
  (* END: Definitions for String library functions *)

  
  (* BEGIN: Definition for List library function *)
  let initList_t = L.function_type void_t [| L.pointer_type list_t; i64_t; i32_t; L.pointer_type i8_t |] in
  let getEl_t = L.function_type void_t [| L.pointer_type list_t; i32_t; L.pointer_type i8_t |] in 
  let addEl_t = L.function_type void_t [| L.pointer_type list_t; L.pointer_type i8_t |] in
  let setEl_t = L.function_type void_t [| L.pointer_type list_t; i32_t; L.pointer_type i8_t |] in
  let popEl_t = L.function_type void_t [| L.pointer_type list_t |] in 
  let lenlist_t = L.function_type i32_t [| L.pointer_type list_t |] in
  let join_t = L.function_type void_t [|L.pointer_type list_t; i8_t ; L.pointer_type string_t|] in
  let make_t = L.function_type void_t [| L.pointer_type list_t; i64_t; i32_t; L.pointer_type i8_t |] in
  let subseq_t = L.function_type void_t [| L.pointer_type list_t; i32_t; i32_t; L.pointer_type list_t|] in

  let initList = L.declare_function "initList" initList_t the_module in
  let getElement= L.declare_function "getElement" getEl_t  the_module in
  let addElement = L.declare_function "addElement" addEl_t the_module in
  let setElement = L.declare_function "setElement" setEl_t  the_module in
  let popElement= L.declare_function "popElement" popEl_t  the_module in
  let lenlist = L.declare_function "lenlist" lenlist_t the_module in
  let join = L.declare_function "join" join_t the_module in
  let make_list = L.declare_function "make" make_t the_module in
  let subseq = L.declare_function "subSequence" subseq_t the_module in
  (* END: Definition for List library function *)
  

  (* print boolean *)
  let printb = L.declare_function "printb" (L.function_type void_t [| i1_t |]) the_module in
  (* print float *)
  (* let printb = L.declare_function "printfl" (L.function_type void_t [| float_t |]) the_module in *)



  (* printf strings *)
  let int_str = L.const_stringz context "%d\n" in
  let float_str = L.const_stringz context "%f\n" in
  let char_str = L.const_stringz context "%c\n" in
  let empty_str = L.const_stringz context "" in

  let int_format_str = L.define_global "fmt_int" int_str the_module in
  let float_format_str = L.define_global "fmt_float" float_str the_module in
  let char_format_str = L.define_global "fmt_char" char_str the_module in
  let empty_initialize = L.define_global "empty_str" empty_str the_module in
  (* End printf strings *)


  (* File IO Functions *)
  let read_t = L.function_type void_t [|L.pointer_type string_t; L.pointer_type string_t|] in
  let write_t = L.function_type void_t [|L.pointer_type string_t; L.pointer_type string_t |] in


  let read = L.declare_function "read" read_t the_module in
  let write = L.declare_function "write" write_t the_module in
  (* End File IO Functions *)



  (* Declare printf function *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in


  
  let get_type ty = 
    let llty = ltype_of_typ ty in
    match ty with
    | String | List _ -> L.pointer_type llty
    | _ -> llty
  in



  let gep_str value index = 
    let pointer = L.const_gep value [| L.const_int i32_t index |] in
    L.const_bitcast pointer (L.pointer_type i8_t) 
  in



  let function_decls : (L.llvalue * sfunc_def) StringMap.t = 
    let function_decl map fdecl = 
      let name = fdecl.sfunc_name 
      and formal_types = Array.of_list (List.map (fun (ty, _) -> get_type ty) fdecl.sparameters) in
      let llvalue = L.function_type (get_type fdecl.sreturn_type) formal_types in
      StringMap.add name (L.define_function name llvalue the_module, fdecl) map 
    in
    List.fold_left function_decl StringMap.empty functions 
  in

  (* LLVM insists each basic block end with exactly one "terminator"
    instruction that transfers control.  This function runs "instr builder"
    if the current block does not already have a terminator.  Used,
    e.g., to handle the "fall off the end of the function" case. *)

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) 
  in

  let function_decls = ref function_decls in
  let globalvars = ref [] in


  let rec build_function_body fdecl = 
    
    let (current_function, _) = StringMap.find fdecl.sfunc_name !function_decls in 

    (* Set containing all of the variable within the current function which should be allocated on the heap *)
    let heap_variables = List.fold_left (fun set el -> Set.add el set) Set.empty fdecl.sheap_vars in
    
    let builder : L.llbuilder = L.builder_at_end context (L.entry_block current_function) in

    let my_builder = ref builder in
    let stale = ref false in


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

    let add_identifier lst (ty, str) = 
      let var = L.define_global str (initialized_value ty) the_module in 
      let _ = match ty with
      | A.String -> ignore(L.build_call initString [|var; empty_str|] "" builder)
      | A.List el_ty -> 
        let ptr = L.build_load (L.build_struct_gep var 2 "" builder) "" builder in
        ignore(L.build_call initList [| var; L.size_of (ltype_of_typ el_ty); L.const_int i32_t 0; ptr |] "" builder)
      | _ -> ()
      in
      (str, var, ty) :: lst
    in
      
    let _ =
      (* Initialize global variables in main function *)
      if fdecl.sfunc_name = "main" then (
        let globalv : (string * L.llvalue * Ast.ty) list = List.fold_left add_identifier [] globals in
        globalvars := globalv;
      ) else ()
    in

    let formals : (string * L.llvalue * Ast.ty) list =
      let add_formal lst (ty, name) param =
        L.set_value_name name param;
        let local = if Set.mem name heap_variables then (
            L.build_malloc (ltype_of_typ ty) name builder  (* Will need to replace with gc malloc *)
          ) else (
            L.build_alloca (ltype_of_typ ty) name builder
          ) 
        in
        let _ = 
          if is_iterable ty then (
            let copy = L.build_load param "" builder in 
            L.build_store copy local builder
          ) else L.build_store param local builder
        in
        (name, local, ty) :: lst
      in
      List.fold_left2 add_formal [] fdecl.sparameters (Array.to_list (L.params current_function))
    in

    let symbol_table =
      List.fold_left (fun accum (name, llv, type_) -> St.add_current_scope accum name (llv, type_)) (St.add_scope (St.empty ())) !globalvars in
    
    let symbol_table = 
      List.fold_left (fun accum (name, llv, type_) -> St.add_current_scope accum name (llv, type_)) (St.add_scope symbol_table) formals in

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
        ignore(L.build_call initString [|global; strptr|] "" builder);
        global
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
        let _ = 
          if is_iterable ty then (
            let ptr = L.build_load (L.build_struct_gep global 2 "" builder) "" builder in
            ignore(L.build_call initList [| global; L.size_of llvty; L.const_int i32_t 0; ptr |] "" builder);
            ignore(List.map add_to_lst llvlst)
          ) else (
            let llvarray = Array.of_list (List.map (build_expr table builder) lst) in (* array of llvm values *)
            let buffer = L.define_global "tmparr" (L.const_array llvty llvarray) the_module in (* Set as global variable *)
            let ptr = L.build_bitcast buffer (L.pointer_type i8_t) "buff_ptr" builder in
            ignore(L.build_call initList [| global; L.size_of llvty; llvlen; ptr |] "" builder)
          )
        in
        global
      | SId s -> 
        let (llv, ty) = St.lookup_identifier s table in
        if is_iterable ty then (
          llv
        ) else L.build_load llv s builder
      | SBinop (e1, op, e2) ->
        let e1' = build_expr table builder e1
        and e2' = build_expr table builder e2 in
        let get_compare int_op float_op = function
          A.Int | A.Char -> L.build_icmp int_op
          | A.Float -> L.build_fcmp float_op
          | _ -> raise Bad_compare
        in

        let get_equality int_op float_op = function
          A.Int | A.Char | A.Bool -> L.build_icmp int_op
          | A.Float -> L.build_fcmp float_op
          | _ -> raise Bad_equal
        in

        let ty = fst e1 in
        let is_str = is_string ty in
        begin match op with
        A.Add -> 
          if is_str then (
            let new_string = L.build_alloca (ltype_of_typ A.String) "result" builder in
            ignore(L.build_call concat [|e1'; e2'; new_string|] "" builder);
            new_string
          ) else (
            L.build_add e1' e2' "tmp" builder
          )
        | A.Sub     -> L.build_sub e1' e2' "tmp" builder
        | A.Mult    -> L.build_mul e1' e2' "tmp" builder
        | A.Div     -> L.build_sdiv e1' e2' "tmp" builder
        | A.Mod     -> L.build_srem e1' e2' "tmp" builder
        | A.And     -> L.build_and e1' e2' "tmp" builder
        | A.Or      -> L.build_or e1' e2' "tmp" builder
        | A.Equal   -> 
          if is_str then (
            let op = initialized_value A.Bool in (* Indicate the type of operation *)
            L.build_call compare [|e1'; e2'; op|] "" builder
          ) else (
            (get_equality L.Icmp.Eq L.Fcmp.Oeq ty) e1' e2' "tmp" builder
          )
        | A.Neq     -> 
          if is_str then (
            let op = L.const_int i1_t 1 in (* Indicate the type of operation *)
            L.build_call compare [|e1'; e2'; op|] "" builder
          ) else (
            (get_equality L.Icmp.Ne L.Fcmp.One ty) e1' e2' "tmp" builder
          )
        | A.Less    -> (get_compare L.Icmp.Slt L.Fcmp.Olt ty) e1' e2' "tmp" builder
        | A.Leq     -> (get_compare L.Icmp.Sle L.Fcmp.Ole ty) e1' e2' "tmp" builder
        | A.Greater -> (get_compare L.Icmp.Sgt L.Fcmp.Ogt ty) e1' e2' "tmp" builder
        | A.Geq     -> (get_compare L.Icmp.Sge L.Fcmp.Oge ty) e1' e2' "tmp" builder
        end

      | SCall (f, args) -> (
          try (* try to see if it's a built in function first *)
            check_built_in f args table builder
          with Not_found ->
            let fdef = (
              try
                fst (StringMap.find f !function_decls)
              with Not_found -> 
                L.build_load (fst (St.lookup_identifier f table)) "" builder
            ) in
            let llargs = List.rev (List.map (build_expr table builder) (List.rev args)) in
            let result = if type_ = Void then "" else f ^ "_result" in
            L.build_call fdef (Array.of_list llargs) result builder 
        )
      | SSeqAccess(s, e) -> 
        let llval = build_expr table builder s in
        let index_llval = build_expr table builder e in
        let (iden_ty, _) = s in
        begin match iden_ty with
        | A.String -> L.build_call getChar [| llval; index_llval |] "idx" builder
        | A.List ty -> let temp = L.build_alloca (ltype_of_typ ty) "tmp_store" builder in
            let generic_ptr = L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder in
            ignore (L.build_call getElement [| llval ; index_llval ; generic_ptr |] "" builder);
            begin match ty with
            | String | List _ -> temp
            | _ -> L.build_load temp "list_item" builder
            end
        | _ -> raise Invalid
        end
      (*| StructCall -> raise Unimplemented
      | StructAccess -> raise Unimplemented
      | StructAssign -> raise Unimplemented *)
      | SFunctionLit (_, lambda) ->
        let formal_types = Array.of_list (List.map (fun (ty, _) -> get_type ty) lambda.sparameters) in
        let llvalue = L.function_type (get_type lambda.sreturn_type) formal_types in
        let llfunc = L.define_function (fdecl.sfunc_name ^ "_anon") llvalue the_module in
        let lambda = {
          sfunc_name = L.value_name llfunc;
          sparameters = lambda.sparameters;
          sreturn_type = lambda.sreturn_type;
          sbody =  lambda.sbody;
          sheap_vars = lambda.sheap_vars;
        }
        in
        function_decls := StringMap.add (L.value_name llfunc) (llfunc, lambda)  !function_decls;
        build_function_body lambda;
        llfunc
      | SSubSeq(l, s, e) ->
        let l' = build_expr table builder l in
        let s' = build_expr table builder s in
        let e' = build_expr table builder e in
        let new_list = L.build_alloca list_t "result" builder in
        ignore(L.build_call subseq [|l'; s'; e'; new_list|] "" builder);
        new_list
      | _ -> raise Unimplemented
    and check_built_in name e table builder =
      let args = 
        match name with
        | "make" -> List.map (build_expr table builder) (List.tl e)
        | _ -> List.map (build_expr table builder) e
      in
      begin match name with
      "println" -> 
        let (ty, _) = List.hd e in
        begin match ty with
        | A.Int -> L.build_call printf_func (Array.of_list ([gep_str int_format_str 0] @ args)) "printf" builder
        | A.Float -> L.build_call printf_func (Array.of_list ([gep_str float_format_str 0] @ args)) "printf" builder
        | A.Char -> L.build_call printf_func (Array.of_list ([gep_str char_format_str 0] @ args)) "printf" builder
        | A.Bool -> L.build_call printb (Array.of_list args) "" builder
        | A.String -> L.build_call prints (Array.of_list args) "" builder
        | _ -> raise Invalid
        end
      | "append" -> 
        let el = List.nth args 1 in
        let (ty, _) = List.nth e 1 in
        let lst = List.hd args in
        let temp = L.build_alloca (ltype_of_typ ty) "" builder in
        let _ = 
          if is_iterable ty then (
            let v = L.build_load el "" builder in 
            L.build_store v temp builder
          ) else L.build_store el temp builder
        in
        let generic = L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder in
        L.build_call addElement [| lst; generic |] "" builder
      | "len" -> 
        let (ty, _) = List.hd e in
        begin match ty with
        A.String -> L.build_call strLength (Array.of_list args) "length" builder
        | A.List _ -> L.build_call lenlist (Array.of_list args) "length" builder
        | _ -> raise Invalid
        end
      | "pop" -> L.build_call popElement (Array.of_list args) "" builder
      | "make" -> 
        let ty = match List.hd e with | (A.Type ty, _) -> ty | _ -> raise Unimplemented in
        let element_size = L.size_of (ltype_of_typ ty) in
        let n = List.nth args 0 in
        let element = List.nth args 1 in
        let result = L.build_alloca (ltype_of_typ (A.List ty)) "result" builder in
        let temp = L.build_alloca (ltype_of_typ ty) "" builder in
        let _ = 
          if is_iterable ty then (
            let v = L.build_load element "" builder in 
            L.build_store v temp builder
          ) else L.build_store element temp builder
        in
        let generic = L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder in
        ignore(L.build_call make_list [| result; element_size; n; generic|] "" builder);
        result
      | "put" -> 
        let el = List.nth args 2 in
        let (ty, _) = List.nth e 2 in
        let idx = List.nth args 1 in
        let lst = List.hd args in
        let temp = L.build_alloca (ltype_of_typ ty) "" builder in
        let _ = 
          if is_iterable ty then (
            let v = L.build_load el "" builder in
            L.build_store v temp builder
          ) else L.build_store el temp builder
        in
        let generic = L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder in
        L.build_call setElement [| lst; idx; generic |] "" builder
      | "map" -> 
        let lst = List.hd args in
        let llfunc = List.nth args 1 in

        let el_ty = get_element_type (fst (List.hd e)) in
        

        let new_ty =
          let get_newlist_type = function
            A.Function (_, output) -> output
            | _ -> raise Invalid
          in
          get_newlist_type (fst (List.nth e 1))
        in

        (* Initialize a new list *)
        let var = L.build_alloca list_t name builder in
        let ptr = L.build_load (L.build_struct_gep var 2 "" builder) "" builder in
        ignore(L.build_call initList [| var; L.size_of (ltype_of_typ new_ty); L.const_int i32_t 0; ptr |] "" builder);

        
        (* Temporary for current list element *)
        let iterable = L.build_alloca (ltype_of_typ el_ty) "" builder in


        (* Allocate temporary variable to store *)
        let temp = L.build_alloca (ltype_of_typ new_ty) name builder in


        (* Get length *)
        let len= L.build_alloca (ltype_of_typ A.Int) "" builder in 
        ignore(L.build_store ( L.build_call lenlist [|lst|] "" builder) len builder);
      

        (* Set counter *)
        let counter = L.build_alloca (ltype_of_typ A.Int) "" builder in
        ignore(L.build_store (initialized_value A.Int) counter builder);
      

        let map_loop = L.append_block context "map" current_function in

        let start_map = L.build_br map_loop in (* partial function *)
        ignore (start_map builder);

        let map_builder = L.builder_at_end context map_loop in

        let llvalue = L.build_icmp L.Icmp.Slt (L.build_load counter "count" map_builder) (L.build_load len "len" map_builder) "" map_builder in
        let map_body = L.append_block context "map_body" current_function in
        let body_builder = L.builder_at_end context map_body in
        let index = L.build_load counter "" body_builder in
        ignore (L.build_call getElement [|lst; index; L.build_bitcast iterable (L.pointer_type i8_t) "" body_builder|] "" body_builder);

        
        let item = (match el_ty with A.String | A.List _ -> iterable | _ -> L.build_load iterable "" body_builder) in
        let result = L.build_call llfunc [|item|] "" body_builder in
        let result = 
          match new_ty with 
          A.String | A.List _ -> L.build_load result "" body_builder
          | _ -> result
        in
        ignore (L.build_store result temp body_builder);
        let generic = L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" body_builder in
        ignore(L.build_call addElement [| var; generic |] "" body_builder);
      
        let add_res = L.build_add (L.build_load counter "" body_builder) (L.const_int i32_t 1) "" body_builder in
        ignore(L.build_store add_res counter body_builder);
        add_terminal body_builder start_map;

        let map_end = L.append_block context "map_end" current_function in

        ignore (L.build_cond_br llvalue map_body map_end map_builder);
        my_builder := (L.builder_at_end context map_end);
        stale := true;
        var
      | "read" -> 
        let buffer = L.build_alloca (ltype_of_typ A.String) "buffer" builder in
        ignore(L.build_call read (Array.of_list (args @ [buffer])) "" builder);
        buffer
      | "write" -> L.build_call write (Array.of_list args) "" builder
      | "split" -> 
        let lst = L.build_alloca (ltype_of_typ (A.List A.String)) "result" builder in
        ignore(L.build_call split (Array.of_list (args @ [lst])) "" builder);
        lst
      | "join" ->
        let str = L.build_alloca (ltype_of_typ A.String) "result" builder in
        ignore(L.build_call join (Array.of_list (args @ [str])) "" builder);
        str
      | _ -> raise Not_found
      end
    in

    let rec build_stmt_list table builder = function
      | [] -> builder
      | stmt :: tail -> 
        let (builder, table) = build_stmt table builder stmt in
          build_stmt_list table builder tail
    and build_stmt table builder = function
      | SBlock lst -> 
          let new_table = St.add_scope table in
            (build_stmt_list new_table builder lst, table)
      | SExpr sexpr -> ignore(build_expr table builder sexpr); (builder, table)
      | SExplicit ((ty, name), sexpr) ->
        let e' = build_expr table builder sexpr in
        let builder = if !stale then (stale := false; !my_builder) else builder in
        let var = L.build_alloca (ltype_of_typ ty) name builder in
        let table = St.add_current_scope table name (var,ty) in
        let _ = 
          if is_iterable ty then (
            let v = L.build_load e' "temp" builder in
            L.build_store v var builder
          ) else L.build_store e' var builder
        in
        (builder, table)
      | SDeclare(ty, name) -> 
        let var = L.build_alloca (ltype_of_typ ty) name builder in
        let _ = 
          begin match ty with
          | String -> L.build_call initString [|var; gep_str empty_initialize 0|] "" builder
          | List el_ty -> 
            let ptr = L.build_load (L.build_struct_gep var 2 "" builder) "" builder in
            L.build_call initList [| var; L.size_of (ltype_of_typ el_ty); L.const_int i32_t 0; ptr |] "" builder
          | _ -> L.build_store (initialized_value ty) var builder
          end
        in
        let table = St.add_current_scope table name (var,ty) in
        (builder, table)
      | SDefine (name, sexpr) -> 
        let e' = build_expr table builder sexpr in
        let builder = if !stale then (stale := false; !my_builder) else builder in
        let ty = fst sexpr in
        let var = L.build_alloca (ltype_of_typ ty) name builder in
        let table = St.add_current_scope table name (var,ty) in
        let _ = 
          if is_iterable ty then (
            let v = L.build_load e' "temp" builder in
            L.build_store v var builder
          ) else L.build_store e' var builder
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
        let (end_then_builder, _) = build_stmt table (L.builder_at_end context if_block) stmt in
        let if_end = L.append_block context "if_end" current_function in
        let build_break = L.build_br if_end in
        add_terminal end_then_builder build_break;
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
        let (end_then_builder, _) = build_stmt table (L.builder_at_end context if_block) stmt1 in
        let else_block = L.append_block context "else_block" current_function in
        let (end_else_builder, _) = build_stmt table (L.builder_at_end context else_block) stmt2 in
        let if_end  = L.append_block context "if_end" current_function in
        let build_break = L.build_br if_end in
        add_terminal end_then_builder build_break;
        add_terminal end_else_builder build_break;
        add_terminal (L.builder_at_end context if_block) build_break;
        add_terminal (L.builder_at_end context else_block) build_break;

        ignore (L.build_cond_br llvalue if_block else_block builder);
        (L.builder_at_end context if_end, table)
      | SAssign (s, (ty, e)) -> 
        let e' = build_expr table builder (ty, e) in
        let var = fst (St.lookup_identifier s table) in
        let _ = 
          if is_iterable ty then (
            let v = L.build_load e' "temp" builder in
            L.build_store v var builder
          ) else L.build_store e' var builder
        in
        (builder, table)
      | SAssignSeq(lst, idx, ex) -> 
        let llv_lst = build_expr table builder lst in
        let llv_idx = build_expr table builder idx in
        let llv_ex = build_expr table builder ex in
        let set_element llval = 
          let generic_ptr = L.build_bitcast llval (L.pointer_type i8_t) "generic" builder in
          L.build_call setElement [| llv_lst; llv_idx ; generic_ptr |] "" builder 
        in
        let _ = 
          let ty = fst ex in
          if is_iterable ty then (
            set_element llv_ex
          ) else (
            let temp = L.build_alloca (ltype_of_typ ty) "" builder in
            ignore(L.build_store llv_ex temp builder);
            set_element temp
          )
        in
        (builder, table)
      | SIterate (name, sexpr, stmt) -> 
        (*
        setup
        for:
          bz count < len
        for_body:
          getElement

        for_end:

        *)
        let e' = build_expr table builder sexpr in
        let ty = fst sexpr in
        let iterable = L.build_alloca (ltype_of_typ ty) "" builder in
        let v = L.build_load e' "temp" builder in 
          ignore(L.build_store v iterable builder);


        (* Allocate temporary variable to store *)
        let el_ty = get_element_type ty in
        let temp = L.build_alloca (ltype_of_typ el_ty) name builder in


        (* Get length *)
        let len= L.build_alloca (ltype_of_typ A.Int) "" builder in
        let _ =  match ty with
          | List _ -> L.build_store ( L.build_call lenlist [|iterable|] "" builder) len builder
          | String -> L.build_store ( L.build_call strLength [|iterable|] "" builder) len builder
          | _ -> raise Invalid
        in


        let add_res = L.build_sub (L.build_load len "" builder) (L.const_int i32_t 0) "" builder in
          ignore(L.build_store add_res len builder);


        (* Set counter *)
        let counter = L.build_alloca (ltype_of_typ A.Int) "" builder in
          ignore(L.build_store (initialized_value A.Int) counter builder);

        let new_table = St.add_scope table in
        let new_table = St.add_current_scope new_table name (temp,el_ty) in
      

        let for_loop = L.append_block context "for" current_function in

        let start_for = L.build_br for_loop in (* partial function *)
        ignore (start_for builder);

        let for_builder = L.builder_at_end context for_loop in

        let llvalue = L.build_icmp L.Icmp.Slt (L.build_load counter "" for_builder) (L.build_load len "" for_builder) "" for_builder in
        let for_body = L.append_block context "for_body" current_function in
        let body_builder = L.builder_at_end context for_body in
        let index = L.build_load counter "" body_builder in
        let _ =  match ty with
          | List _ -> L.build_call getElement [|iterable; index; L.build_bitcast temp (L.pointer_type i8_t) "" body_builder|] "" body_builder
          | String -> L.build_store (L.build_call getChar [|iterable; index|] "" body_builder) temp body_builder
          | _ -> raise Invalid
        in
        let flatten_body = function
          | SBlock lst -> build_stmt_list new_table body_builder lst
          | _ -> raise Invalid
        in
        let updated_builder = flatten_body stmt in
        let add_res = L.build_add (L.build_load counter "" updated_builder) (L.const_int i32_t 1) "" updated_builder in
          ignore(L.build_store add_res counter updated_builder);
        add_terminal updated_builder start_for;

        let for_end = L.append_block context "for_end" current_function in

        ignore (L.build_cond_br llvalue for_body for_end for_builder);
        
        (L.builder_at_end context for_end, table)
      | SRange(name, sexpr1, sexpr2, stmt) ->
        let e1' = build_expr table builder sexpr1 in
        let hi = build_expr table builder sexpr2 in


        (* Allocate temporary variable to store *)
        let low = L.build_alloca (ltype_of_typ A.Int) name builder in
        ignore(L.build_store e1' low builder);


        let new_table = St.add_scope table in
        let new_table = St.add_current_scope new_table name (low, A.Int) in
      

        let for_loop = L.append_block context "for" current_function in

        let start_for = L.build_br for_loop in (* partial function *)
        ignore (start_for builder);

        let for_builder = L.builder_at_end context for_loop in

        let llvalue = L.build_icmp L.Icmp.Slt (L.build_load low "" for_builder) hi "" for_builder in
        let for_body = L.append_block context "for_body" current_function in
        let body_builder = L.builder_at_end context for_body in
        let flatten_body = function
          | SBlock lst -> build_stmt_list new_table body_builder lst
          | _ -> raise Invalid
        in
        let updated_builder = flatten_body stmt in
        let add_res = L.build_add (L.build_load low "" updated_builder) (L.const_int i32_t 1) "" updated_builder in
          ignore(L.build_store add_res low updated_builder);
        add_terminal updated_builder start_for;

        let for_end = L.append_block context "for_end" current_function in

        ignore (L.build_cond_br llvalue for_body for_end for_builder);
        
        (L.builder_at_end context for_end, table)
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
        let _ = begin match ty with 
          | Void -> L.build_ret_void builder
          | _ ->
          let e = build_expr table builder sexpr in
          let return_val = 
            if is_iterable ty then (
              let mem = L.build_malloc (ltype_of_typ ty) "return" builder in (* need to malloc so that object one roll back in stack*)
              ignore(L.build_store (L.build_load e "" builder) mem builder);
              mem
            ) else e
          in
          L.build_ret return_val builder
        end
        in
        (builder, table)
      | _ -> raise Unimplemented
    in
    
    let funcbuilder = match fdecl.sbody with
    | SBlock lst -> build_stmt_list symbol_table builder lst (* Flatten sstmt *)
    | _ -> raise Invalid
    in
    
    if fdecl.sreturn_type = Void then add_terminal funcbuilder (L.build_ret_void)
  in
  List.iter build_function_body functions;
  the_module
