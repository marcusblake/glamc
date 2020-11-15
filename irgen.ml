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
open Print_ast
module L = Llvm
module A = Ast
module St = Symbol_table
module StringMap = Map.Make (String)
module Set = Set.Make (String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions, structs) =
  let context = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "GlamC" in

  (* Get types from the context *)
  let i32_t = L.i32_type context
  and i64_t = L.i64_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and float_t = L.float_type context
  and void_t = L.void_type context
  and string_t = L.named_struct_type context "string"
  and list_t = L.named_struct_type context "list"
  and structs_t =
    List.fold_left
      (fun map struct_ ->
        let name = struct_.sstruct_name in
        StringMap.add name (L.named_struct_type context name) map)
      StringMap.empty structs
  in

  (* Return the LLVM type for a GlamC type *)
  let rec lltype_of_type = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Char -> i8_t
    | A.String -> L.pointer_type string_t
    | A.List _ -> L.pointer_type list_t
    | A.Function (params, return) ->
        let parameters = Array.of_list (List.map lltype_of_type params) in
        let return_type = lltype_of_type return in
        L.pointer_type (L.function_type return_type parameters)
    | A.Void -> void_t
    | A.Struct name -> L.pointer_type (StringMap.find name structs_t)
    | _ -> raise Invalid
  in

  let lltype_of_struct_types = function
    | A.String -> string_t
    | A.List _ -> list_t
    | A.Struct name -> StringMap.find name structs_t
    | _ -> raise Invalid
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
    ignore (L.struct_set_body string_t [| i32_t; L.pointer_type i8_t |] false);
    ignore
      (L.struct_set_body list_t
         [| i32_t; i32_t; i32_t; L.pointer_type i8_t |]
         false);
    List.iter
      (fun struct_ ->
        let struct_name = struct_.sstruct_name in
        let llv_struct_type = StringMap.find struct_name structs_t in
        let get_property (ty, _) = lltype_of_type ty in
        let properties = List.map get_property struct_.sfields in
        ignore
          (L.struct_set_body llv_struct_type (Array.of_list properties) false))
      structs
  in

  let gmalloc_t = L.function_type (L.pointer_type i8_t) [| i64_t |] in
  let gmalloc = L.declare_function "gmalloc" gmalloc_t the_module in

  (* BEGIN: Definitions for String library functions *)
  let initString_t =
    L.function_type void_t [| L.pointer_type string_t; L.pointer_type i8_t |]
  in
  let strLength_t = L.function_type i32_t [| L.pointer_type string_t |] in
  let getChar_t = L.function_type i8_t [| L.pointer_type string_t; i32_t |] in
  let prints_t = L.function_type void_t [| L.pointer_type string_t |] in
  let concat_t =
    L.function_type void_t
      [|
        L.pointer_type string_t;
        L.pointer_type string_t;
        L.pointer_type string_t;
      |]
  in
  let split_t =
    L.function_type void_t
      [| L.pointer_type string_t; i8_t; L.pointer_type list_t |]
  in
  let compare_t =
    L.function_type i1_t
      [| L.pointer_type string_t; L.pointer_type string_t; i1_t |]
  in

  let initString = L.declare_function "initString" initString_t the_module in
  let strLength = L.declare_function "lenstr" strLength_t the_module in
  let getChar = L.declare_function "getChar" getChar_t the_module in
  let prints = L.declare_function "prints" prints_t the_module in
  let concat = L.declare_function "concat" concat_t the_module in
  let split = L.declare_function "split" split_t the_module in
  let compare = L.declare_function "compare_string" compare_t the_module in

  (* END: Definitions for String library functions *)

  (* BEGIN: Definition for List library function *)
  let initList_t =
    L.function_type void_t
      [| L.pointer_type list_t; i64_t; i32_t; L.pointer_type i8_t |]
  in
  let getEl_t =
    L.function_type void_t
      [| L.pointer_type list_t; i32_t; L.pointer_type i8_t |]
  in
  let addEl_t =
    L.function_type void_t [| L.pointer_type list_t; L.pointer_type i8_t |]
  in
  let setEl_t =
    L.function_type void_t
      [| L.pointer_type list_t; i32_t; L.pointer_type i8_t |]
  in
  let popEl_t = L.function_type void_t [| L.pointer_type list_t |] in
  let lenlist_t = L.function_type i32_t [| L.pointer_type list_t |] in
  let join_t =
    L.function_type void_t
      [| L.pointer_type list_t; i8_t; L.pointer_type string_t |]
  in
  let make_t =
    L.function_type void_t
      [| L.pointer_type list_t; i64_t; i32_t; L.pointer_type i8_t |]
  in
  let subseq_t =
    L.function_type void_t
      [| L.pointer_type list_t; i32_t; i32_t; L.pointer_type list_t |]
  in

  let initList = L.declare_function "initList" initList_t the_module in
  let getElement = L.declare_function "getElement" getEl_t the_module in
  let addElement = L.declare_function "addElement" addEl_t the_module in
  let setElement = L.declare_function "setElement" setEl_t the_module in
  let popElement = L.declare_function "popElement" popEl_t the_module in
  let lenlist = L.declare_function "lenlist" lenlist_t the_module in
  let join = L.declare_function "join" join_t the_module in
  let make_list = L.declare_function "make" make_t the_module in
  let subseq = L.declare_function "subSequence" subseq_t the_module in

  (* END: Definition for List library function *)

  (* print boolean *)
  let printb =
    L.declare_function "printb" (L.function_type void_t [| i1_t |]) the_module
  in

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
  let read_t =
    L.function_type void_t
      [| L.pointer_type string_t; L.pointer_type string_t |]
  in
  let write_t =
    L.function_type void_t
      [| L.pointer_type string_t; L.pointer_type string_t |]
  in

  let read = L.declare_function "read" read_t the_module in
  let write = L.declare_function "write" write_t the_module in

  (* End File IO Functions *)

  (* Declare printf function *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in

  let gep_str value index =
    let pointer = L.const_gep value [| L.const_int i32_t index |] in
    L.const_bitcast pointer (L.pointer_type i8_t)
  in

  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl map fdecl =
      let name = fdecl.sfunc_name
      and formal_types =
        Array.of_list
          (List.map (fun (ty, _) -> lltype_of_type ty) fdecl.sparameters)
      in
      let llvalue =
        L.function_type (lltype_of_type fdecl.sreturn_type) formal_types
      in
      StringMap.add name (L.define_function name llvalue the_module, fdecl) map
    in
    List.fold_left function_decl StringMap.empty functions
  in

  let function_decls = ref function_decls in

  (* Map struct to functions <struct_name <struct_function, (llvalue, function_definition>>*)
  let struct_functions : (L.llvalue * sfunc_def) StringMap.t StringMap.t =
    let get_functions map struct_ =
      let struct_name = struct_.sstruct_name in
      let struct_function_decl map fdecl =
        let name = fdecl.sfunc_name in
        (* Inlcude struct in function parameters *)
        let func_parameters =
          [ (A.Struct struct_name, "this") ] @ fdecl.sparameters
        in
        let formal_types =
          Array.of_list
            (List.map (fun (ty, _) -> lltype_of_type ty) func_parameters)
        in
        let llvalue =
          L.function_type (lltype_of_type fdecl.sreturn_type) formal_types
        in
        let func_name = struct_name ^ "_" ^ name in
        let tuple =
          ( L.define_function (struct_name ^ "_" ^ name) llvalue the_module,
            fdecl )
        in
        function_decls := StringMap.add func_name tuple !function_decls;
        StringMap.add name tuple map
      in
      StringMap.add struct_name
        (List.fold_left struct_function_decl StringMap.empty struct_.smethods)
        map
    in
    List.fold_left get_functions StringMap.empty structs
  in

  (* Map the struct field to the index within struct so that we can access fields <struct_name, <field_name, index>>*)
  let struct_fields : (int * Ast.ty) StringMap.t StringMap.t =
    let get_fields map struct_ =
      let indices =
        List.mapi (fun i (ty, name) -> (name, ty, i)) struct_.sfields
      in
      let property_map =
        List.fold_left
          (fun map (name, ty, index) -> StringMap.add name (index, ty) map)
          StringMap.empty indices
      in
      StringMap.add struct_.sstruct_name property_map map
    in
    List.fold_left get_fields StringMap.empty structs
  in

  (* LLVM insists each basic block end with exactly one "terminator"
     instruction that transfers control.  This function runs "instr builder"
     if the current block does not already have a terminator.  Used,
     e.g., to handle the "fall off the end of the function" case. *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in

  let globalvars = ref [] in

  (* Return an initialized value of a given type *)
  let rec initialized_value = function
    | A.Int -> L.const_int i32_t 0
    | A.Bool -> L.const_int i1_t 0 (* Initialize to false *)
    | A.Float -> L.const_float float_t 0.0
    | A.Char -> L.const_int i8_t 0
    | A.String ->
        L.const_named_struct string_t
          [| L.const_int i32_t 0; L.const_pointer_null i8_t |]
    | A.List _ ->
        L.const_named_struct list_t
          [|
            L.const_int i32_t 0; L.const_int i32_t 0; L.const_pointer_null i8_t;
          |]
    | A.Struct name ->
        let struct_ =
          List.find (fun struct_ -> struct_.sstruct_name = name) structs
        in
        let initial_values =
          List.map (fun (ty, _) -> initialized_value ty) struct_.sfields
        in
        L.const_named_struct
          (StringMap.find name structs_t)
          (Array.of_list initial_values)
    | _ -> raise Unimplemented
  in

  let rec build_function_body lookup_identifier fdecl =
    let current_function, _ = StringMap.find fdecl.sfunc_name !function_decls in

    let builder : L.llbuilder =
      L.builder_at_end context (L.entry_block current_function)
    in

    let my_builder = ref builder in
    let stale = ref false in

    let add_identifier lst (ty, str) =
      let var = L.define_global str (initialized_value ty) the_module in
      let _ =
        match ty with
        | A.String ->
            ignore (L.build_call initString [| var; empty_str |] "" builder)
        | A.List el_ty ->
            let ptr =
              L.build_load (L.build_struct_gep var 2 "" builder) "" builder
            in
            let llvm_type = lltype_of_type el_ty in
            ignore
              (L.build_call initList
                 [| var; L.size_of llvm_type; L.const_int i32_t 0; ptr |]
                 "" builder)
        | _ -> ()
      in
      (str, var, ty) :: lst
    in

    let _ =
      (* Initialize global variables in main function. Initialize garbage collection as well later *)
      if fdecl.sfunc_name = "main" then
        let globalv : (string * L.llvalue * Ast.ty) list =
          List.fold_left add_identifier [] globals
        in
        globalvars := globalv
      else ()
    in

    let formals : (string * L.llvalue * Ast.ty) list =
      let add_formal lst (ty, name) param =
        L.set_value_name name param;
        let local = L.build_alloca (lltype_of_type ty) name builder in
        ignore (L.build_store param local builder);
        (name, local, ty) :: lst
      in
      List.fold_left2 add_formal [] fdecl.sparameters
        (Array.to_list (L.params current_function))
    in

    let symbol_table =
      List.fold_left
        (fun accum (name, llv, type_) ->
          St.add_current_scope accum name (llv, type_))
        (St.add_scope (St.empty ()))
        !globalvars
    in

    let symbol_table =
      List.fold_left
        (fun accum (name, llv, type_) ->
          St.add_current_scope accum name (llv, type_))
        (St.add_scope symbol_table)
        formals
    in

    (* TODO: IMPLEMENT BUILD_EXPR -> Needs to return llvalue for expression *)
    let rec build_expr table builder ((type_, e) : sexpr) =
      match e with
      | SIntLit int_literal -> L.const_int i32_t int_literal
      | SBoolLit bool_literal -> L.const_int i1_t (if bool_literal then 1 else 0)
      | SFloatLit float_literal -> L.const_float float_t float_literal
      | SCharLit char_literal -> L.const_int i8_t (int_of_char char_literal)
      | SStringLit string_literal ->
          let strptr =
            L.build_global_stringptr string_literal "string_const" builder
          in
          (* Allocate on heap*)
          let pointer =
            L.build_call gmalloc [| L.size_of string_t |] "alloc" builder
          in
          let string_struct =
            L.build_bitcast pointer (L.pointer_type string_t) "string" builder
          in
          ignore
            (L.build_call initString [| string_struct; strptr |] "" builder);
          string_struct
      | SSeq seq ->
          let llvm_type_list = List.map (build_expr table builder) seq in
          let seq_elem_type =
            match type_ with List ty -> ty | _ -> raise Invalid
          in

          let llvm_seq_elem_type = lltype_of_type seq_elem_type in
          let llvm_list_length = L.const_int i32_t (List.length seq) in

          (* Dynamically allocate memory on heap *)
          let pointer =
            L.build_call gmalloc [| L.size_of list_t |] "alloc" builder
          in
          let list_struct =
            L.build_bitcast pointer (L.pointer_type list_t) "list" builder
          in

          let _ =
            if is_pointer_type seq_elem_type then (
              let temp_store =
                L.build_alloca llvm_seq_elem_type "temp" builder
              in
              let append_to_list element =
                ignore (L.build_store element temp_store builder);
                let generic_element =
                  L.build_bitcast temp_store (L.pointer_type i8_t) "generic"
                    builder
                in
                ignore
                  (L.build_call addElement
                     [| list_struct; generic_element |]
                     "" builder)
              in
              let pointer_size = L.size_of llvm_seq_elem_type in
              let zero_length = L.const_int i32_t 0 in
              ignore
                (L.build_call initList
                   [| list_struct; pointer_size; zero_length; pointer |]
                   "" builder);
              List.iter append_to_list llvm_type_list )
            else
              let const_array =
                L.const_array llvm_seq_elem_type (Array.of_list llvm_type_list)
              in
              let const_global_array =
                L.define_global "global_temp" const_array the_module
              in
              let array_pointer =
                L.build_bitcast const_global_array (L.pointer_type i8_t)
                  "buffer" builder
              in
              let llvm_seq_elem_size = L.size_of llvm_seq_elem_type in
              ignore
                (L.build_call initList
                   [|
                     list_struct;
                     llvm_seq_elem_size;
                     llvm_list_length;
                     array_pointer;
                   |]
                   "" builder)
          in
          list_struct
      | SId s ->
          let llvm_value, _ = lookup_identifier s table builder in
          L.build_load llvm_value s builder
      | SBinop (e1, op, e2) -> (
          let e1' = build_expr table builder e1
          and e2' = build_expr table builder e2 in
          let get_compare int_op float_op = function
            | A.Int | A.Char -> L.build_icmp int_op
            | A.Float -> L.build_fcmp float_op
            | _ -> raise Bad_compare
          in

          let get_equality int_op float_op = function
            | A.Int | A.Char | A.Bool -> L.build_icmp int_op
            | A.Float -> L.build_fcmp float_op
            | _ -> raise Bad_equal
          in

          let ty = fst e1 in
          let is_str = is_string ty in
          match op with
          | A.Add ->
              if is_str then (
                let pointer =
                  L.build_call gmalloc [| L.size_of string_t |] "alloc" builder
                in
                let new_string =
                  L.build_bitcast pointer (lltype_of_type A.String) "string"
                    builder
                in
                ignore
                  (L.build_call concat [| e1'; e2'; new_string |] "" builder);
                new_string )
              else L.build_add e1' e2' "tmp" builder
          | A.Sub -> L.build_sub e1' e2' "tmp" builder
          | A.Mult -> L.build_mul e1' e2' "tmp" builder
          | A.Div -> L.build_sdiv e1' e2' "tmp" builder
          | A.Mod -> L.build_srem e1' e2' "tmp" builder
          | A.And -> L.build_and e1' e2' "tmp" builder
          | A.Or -> L.build_or e1' e2' "tmp" builder
          | A.Equal ->
              if is_str then
                let op = initialized_value A.Bool in
                (* Indicate the type of operation *)
                L.build_call compare [| e1'; e2'; op |] "" builder
              else (get_equality L.Icmp.Eq L.Fcmp.Oeq ty) e1' e2' "tmp" builder
          | A.Neq ->
              if is_str then
                let op = L.const_int i1_t 1 in
                (* Indicate the type of operation *)
                L.build_call compare [| e1'; e2'; op |] "" builder
              else (get_equality L.Icmp.Ne L.Fcmp.One ty) e1' e2' "tmp" builder
          | A.Less ->
              (get_compare L.Icmp.Slt L.Fcmp.Olt ty) e1' e2' "tmp" builder
          | A.Leq ->
              (get_compare L.Icmp.Sle L.Fcmp.Ole ty) e1' e2' "tmp" builder
          | A.Greater ->
              (get_compare L.Icmp.Sgt L.Fcmp.Ogt ty) e1' e2' "tmp" builder
          | A.Geq ->
              (get_compare L.Icmp.Sge L.Fcmp.Oge ty) e1' e2' "tmp" builder )
      | SStructCall (struct_, func_name, arguments) ->
          let llv, ty = lookup_identifier struct_ table builder in
          let struct_name =
            match ty with A.Struct name -> name | _ -> raise Invalid
          in
          let my_functions = StringMap.find struct_name struct_functions in
          let function_def = fst (StringMap.find func_name my_functions) in
          let llargs = List.map (build_expr table builder) arguments in
          let result = if type_ = Void then "" else func_name ^ "_result" in
          L.build_call function_def
            (Array.of_list ([ L.build_load llv "" builder ] @ llargs))
            result builder
      | SCall (f, args) -> (
          try
            (* try to see if it's a built in function first *)
            check_built_in f args table builder
          with Not_found ->
            let fdef =
              try fst (StringMap.find f !function_decls)
              with Not_found ->
                L.build_load
                  (fst (lookup_identifier f table builder))
                  "" builder
            in
            let llargs =
              (* WHY IS THIS REVERSED???? *)
              List.rev (List.map (build_expr table builder) (List.rev args))
            in
            let result = if type_ = Void then "" else f ^ "_result" in
            L.build_call fdef (Array.of_list llargs) result builder )
      | SSeqAccess (seq, expr) -> (
          let seq_ref = build_expr table builder seq in
          let index_llval = build_expr table builder expr in
          let iden_ty, _ = seq in
          match iden_ty with
          | A.String ->
              L.build_call getChar [| seq_ref; index_llval |] "idx" builder
          | A.List ty ->
              let temp =
                L.build_alloca (lltype_of_type ty) "tmp_store" builder
              in
              let generic_ptr =
                L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder
              in
              ignore
                (L.build_call getElement
                   [| seq_ref; index_llval; generic_ptr |]
                   "" builder);
              L.build_load temp "element" builder
          | _ -> raise Invalid )
      | SStructAccess (struct_, field_name) ->
          let llv, ty = lookup_identifier struct_ table builder in
          let struct_name =
            match ty with Struct name -> name | _ -> raise Invalid
          in
          let my_properties = StringMap.find struct_name struct_fields in
          let field_index, _ = StringMap.find field_name my_properties in
          let field_ptr =
            L.build_struct_gep (L.build_load llv "" builder) field_index field_name builder
          in
          L.build_load field_ptr field_name builder
      (* | StructAssign -> raise Unimplemented *)
      | SFunctionLit lambda ->
          let formal_types =
            Array.of_list
              (List.map (fun (ty, _) -> lltype_of_type ty) lambda.sparameters)
          in
          let llvalue =
            L.function_type (lltype_of_type lambda.sreturn_type) formal_types
          in
          let llfunc =
            L.define_function (fdecl.sfunc_name ^ "_anon") llvalue the_module
          in
          let lambda =
            {
              sfunc_name = L.value_name llfunc;
              sparameters = lambda.sparameters;
              sreturn_type = lambda.sreturn_type;
              sbody = lambda.sbody;
            }
          in
          function_decls :=
            StringMap.add (L.value_name llfunc) (llfunc, lambda) !function_decls;
          build_function_body lookup_identifier lambda;
          llfunc
      | SSubSeq (source_list, seq_start, seq_end) ->
          let llvm_list = build_expr table builder source_list in
          let llvm_start = build_expr table builder seq_start in
          let llvm_end = build_expr table builder seq_end in
          (* Dynamically allocate memory on heap *)
          let pointer =
            L.build_call gmalloc [| L.size_of list_t |] "alloc" builder
          in
          let new_list =
            L.build_bitcast pointer (L.pointer_type list_t) "new_list" builder
          in
          ignore
            (L.build_call subseq
               [| llvm_list; llvm_start; llvm_end; new_list |]
               "" builder);
          new_list
      | _ ->
          ignore (Printf.printf "irgen expr");
          raise Unimplemented
    and check_built_in name e table builder =
      let args =
        match name with
        | "make" -> List.map (build_expr table builder) (List.tl e)
        | _ -> List.map (build_expr table builder) e
      in
      match name with
      | "println" -> (
          let ty, _ = List.hd e in
          match ty with
          | A.Int ->
              L.build_call printf_func
                (Array.of_list ([ gep_str int_format_str 0 ] @ args))
                "printf" builder
          | A.Float ->
              L.build_call printf_func
                (Array.of_list ([ gep_str float_format_str 0 ] @ args))
                "printf" builder
          | A.Char ->
              L.build_call printf_func
                (Array.of_list ([ gep_str char_format_str 0 ] @ args))
                "printf" builder
          | A.Bool -> L.build_call printb (Array.of_list args) "" builder
          | A.String -> L.build_call prints (Array.of_list args) "" builder
          | _ -> raise Invalid )
      | "append" ->
          let el = List.nth args 1 in
          let ty, _ = List.nth e 1 in
          let lst = List.hd args in
          let temp = L.build_alloca (lltype_of_type ty) "" builder in
          ignore (L.build_store el temp builder);

          let generic =
            L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder
          in
          L.build_call addElement [| lst; generic |] "" builder
      | "len" -> (
          let ty, _ = List.hd e in
          match ty with
          | A.String ->
              L.build_call strLength (Array.of_list args) "length" builder
          | A.List _ ->
              L.build_call lenlist (Array.of_list args) "length" builder
          | _ -> raise Invalid )
      | "pop" -> L.build_call popElement (Array.of_list args) "" builder
      | "make" ->
          let ty =
            match List.hd e with A.Type ty, _ -> ty | _ -> raise Unimplemented
          in
          let llvm_type = lltype_of_type ty in
          let element_size = L.size_of llvm_type in
          let n = List.nth args 0 in
          let element = List.nth args 1 in
          let pointer =
            L.build_call gmalloc [| L.size_of list_t |] "alloc" builder
          in
          let result =
            L.build_bitcast pointer (L.pointer_type list_t) "result" builder
          in
          let temp = L.build_alloca (lltype_of_type ty) "" builder in
          ignore (L.build_store element temp builder);
          let generic =
            L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder
          in
          ignore
            (L.build_call make_list
               [| result; element_size; n; generic |]
               "" builder);
          result
      | "put" ->
          let el = List.nth args 2 in
          let ty, _ = List.nth e 2 in
          let idx = List.nth args 1 in
          let lst = List.hd args in
          let llvm_type = lltype_of_type ty in
          let temp = L.build_alloca llvm_type "" builder in
          ignore (L.build_store el temp builder);
          let generic =
            L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" builder
          in
          L.build_call setElement [| lst; idx; generic |] "" builder
      | "map" ->
          let lst = List.hd args in
          let llfunc = List.nth args 1 in

          let el_ty = get_element_type (fst (List.hd e)) in

          let new_ty =
            let get_newlist_type = function
              | A.Function (_, output) -> output
              | _ -> raise Invalid
            in
            get_newlist_type (fst (List.nth e 1))
          in

          let llvm_newtype = lltype_of_type new_ty in
          (* Initialize a new list *)
          let var = L.build_call gmalloc [| L.size_of list_t |] name builder in
          ignore
            (L.build_call initList
               [| var; L.size_of llvm_newtype; L.const_int i32_t 0; var |]
               "" builder);

          (* Temporary for current list element *)
          let iterable = L.build_alloca (lltype_of_type el_ty) "" builder in

          (* Allocate temporary variable to store *)
          let temp = L.build_alloca (lltype_of_type new_ty) name builder in

          (* Get length *)
          let len = L.build_alloca (lltype_of_type A.Int) "" builder in
          ignore
            (L.build_store
               (L.build_call lenlist [| lst |] "" builder)
               len builder);

          (* Set counter *)
          let counter = L.build_alloca (lltype_of_type A.Int) "" builder in
          ignore (L.build_store (initialized_value A.Int) counter builder);

          let map_loop = L.append_block context "map" current_function in

          let start_map = L.build_br map_loop in
          (* partial function *)
          ignore (start_map builder);

          let map_builder = L.builder_at_end context map_loop in

          let llvalue =
            L.build_icmp L.Icmp.Slt
              (L.build_load counter "count" map_builder)
              (L.build_load len "len" map_builder)
              "" map_builder
          in
          let map_body = L.append_block context "map_body" current_function in
          let body_builder = L.builder_at_end context map_body in
          let index = L.build_load counter "" body_builder in
          ignore
            (L.build_call getElement
               [|
                 lst;
                 index;
                 L.build_bitcast iterable (L.pointer_type i8_t) "" body_builder;
               |]
               "" body_builder);

          let item = L.build_load iterable "" body_builder in

          let result = L.build_call llfunc [| item |] "" body_builder in

          ignore (L.build_store result temp body_builder);

          let generic =
            L.build_bitcast temp (L.pointer_type i8_t) "buff_ptr" body_builder
          in

          ignore (L.build_call addElement [| var; generic |] "" body_builder);

          let add_res =
            L.build_add
              (L.build_load counter "" body_builder)
              (L.const_int i32_t 1) "" body_builder
          in
          ignore (L.build_store add_res counter body_builder);
          add_terminal body_builder start_map;

          let map_end = L.append_block context "map_end" current_function in

          ignore (L.build_cond_br llvalue map_body map_end map_builder);
          my_builder := L.builder_at_end context map_end;
          stale := true;
          var
      | "read" ->
          let llvm_type = lltype_of_type A.String in
          let buffer =
            L.build_call gmalloc [| L.size_of string_t |] "buffer" builder
          in
          let string_struct =
            L.build_bitcast buffer llvm_type "string_struct" builder
          in
          ignore
            (L.build_call read
               (Array.of_list (args @ [ string_struct ]))
               "" builder);
          string_struct
      | "write" -> L.build_call write (Array.of_list args) "" builder
      | "split" ->
          let llvm_type = lltype_of_type (A.List A.String) in
          let pointer =
            L.build_call gmalloc [| L.size_of list_t |] "pointer" builder
          in
          let list_struct =
            L.build_bitcast pointer llvm_type "string_struct" builder
          in
          ignore
            (L.build_call split
               (Array.of_list (args @ [ list_struct ]))
               "" builder);
          list_struct
      | "join" ->
          let llvm_type = lltype_of_type A.String in
          let buffer =
            L.build_call gmalloc [| L.size_of string_t |] "buffer" builder
          in
          let string_struct =
            L.build_bitcast buffer llvm_type "string_struct" builder
          in
          ignore
            (L.build_call join
               (Array.of_list (args @ [ string_struct ]))
               "" builder);
          string_struct
      | _ -> raise Not_found
    in

    let rec build_stmt_list table builder = function
      | [] -> builder
      | stmt :: tail ->
          let builder, table = build_stmt table builder stmt in
          build_stmt_list table builder tail
    and build_stmt table builder = function
      | SBlock lst ->
          let new_table = St.add_scope table in
          (build_stmt_list new_table builder lst, table)
      | SExpr sexpr ->
          ignore (build_expr table builder sexpr);
          (builder, table)
      | SExplicit ((ty, name), sexpr) ->
          let e' = build_expr table builder sexpr in
          let builder =
            if !stale then (
              stale := false;
              !my_builder )
            else builder
          in
          let llvm_type = lltype_of_type ty in
          let var = L.build_alloca llvm_type name builder in
          let table = St.add_current_scope table name (var, ty) in
          ignore (L.build_store e' var builder);
          (builder, table)
      | SDeclare (ty, name) ->
          let llvm_type = lltype_of_type ty in
          let variable = L.build_alloca llvm_type name builder in
          let initial_value =
            if is_pointer_type ty then (
              let struct_type = lltype_of_struct_types ty in
              let pointer =
                L.build_call gmalloc [| L.size_of struct_type |] "" builder
              in
              let pointer = 
                L.build_bitcast pointer llvm_type "variable" builder
              in
              let _ =
                match ty with
                | String ->
                    L.build_call initString
                      [| pointer; gep_str empty_initialize 0 |]
                      "" builder
                | List element_type ->
                    let llvm_type = lltype_of_type element_type in
                    L.build_call initList
                      [|
                        pointer; L.size_of llvm_type; L.const_int i32_t 0; gep_str empty_initialize 0;
                      |]
                      "" builder
                | Struct _ -> pointer (* Can call init function as some later *)
                | _ -> 
                  let error_msg = string_of_typ ty ^ " is not a pointer type" in
                  raise (IRGenerationError(error_msg))
              in
              pointer
            ) else initialized_value ty
          in
          ignore (L.build_store initial_value variable builder);
          let table = St.add_current_scope table name (variable, ty) in
          (builder, table)
      | SDefine (name, sexpr) ->
          let e' = build_expr table builder sexpr in
          let builder =
            if !stale then (
              stale := false;
              !my_builder )
            else builder
          in
          let ty = fst sexpr in
          let llvm_type = lltype_of_type ty in
          let var = L.build_alloca llvm_type name builder in
          let table = St.add_current_scope table name (var, ty) in
          ignore (L.build_store e' var builder);
          (builder, table)
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
          let end_then_builder, _ =
            build_stmt table (L.builder_at_end context if_block) stmt
          in
          let if_end = L.append_block context "if_end" current_function in
          let build_break = L.build_br if_end in
          add_terminal end_then_builder build_break;
          add_terminal (L.builder_at_end context if_block) build_break;

          ignore (L.build_cond_br llvalue if_block if_end builder);
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
          let end_then_builder, _ =
            build_stmt table (L.builder_at_end context if_block) stmt1
          in
          let else_block =
            L.append_block context "else_block" current_function
          in
          let end_else_builder, _ =
            build_stmt table (L.builder_at_end context else_block) stmt2
          in
          let if_end = L.append_block context "if_end" current_function in
          let build_break = L.build_br if_end in
          add_terminal end_then_builder build_break;
          add_terminal end_else_builder build_break;
          add_terminal (L.builder_at_end context if_block) build_break;
          add_terminal (L.builder_at_end context else_block) build_break;

          ignore (L.build_cond_br llvalue if_block else_block builder);
          (L.builder_at_end context if_end, table)
      | SAssign (s, (ty, e)) ->
          let e' = build_expr table builder (ty, e) in
          let var = fst (lookup_identifier s table builder) in
          ignore (L.build_store e' var builder);
          (builder, table)
      | SAssignSeq (lst, idx, ex) ->
          let llv_lst = build_expr table builder lst in
          let llv_idx = build_expr table builder idx in
          let llv_ex = build_expr table builder ex in
          let set_element llval =
            let generic_ptr =
              L.build_bitcast llval (L.pointer_type i8_t) "generic" builder
            in
            L.build_call setElement
              [| llv_lst; llv_idx; generic_ptr |]
              "" builder
          in
          let _ =
            let ty = fst ex in
            let temp = L.build_alloca (lltype_of_type ty) "" builder in
            ignore (L.build_store llv_ex temp builder);
            set_element temp
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
          let iterable = build_expr table builder sexpr in
          let ty = fst sexpr in

          (* Allocate temporary variable to store *)
          let el_ty = get_element_type ty in
          let temp = L.build_alloca (lltype_of_type el_ty) name builder in

          (* Get length *)
          let len = L.build_alloca (lltype_of_type A.Int) "" builder in
          let _ =
            match ty with
            | List _ ->
                L.build_store
                  (L.build_call lenlist [| iterable |] "" builder)
                  len builder
            | String ->
                L.build_store
                  (L.build_call strLength [| iterable |] "" builder)
                  len builder
            | _ -> raise Invalid
          in

          let add_res =
            L.build_sub
              (L.build_load len "" builder)
              (L.const_int i32_t 0) "" builder
          in
          ignore (L.build_store add_res len builder);

          (* Set counter *)
          let counter = L.build_alloca (lltype_of_type A.Int) "" builder in
          ignore (L.build_store (initialized_value A.Int) counter builder);

          let new_table = St.add_scope table in
          let new_table = St.add_current_scope new_table name (temp, el_ty) in

          let for_loop = L.append_block context "for" current_function in

          let start_for = L.build_br for_loop in
          (* partial function *)
          ignore (start_for builder);

          let for_builder = L.builder_at_end context for_loop in

          let llvalue =
            L.build_icmp L.Icmp.Slt
              (L.build_load counter "" for_builder)
              (L.build_load len "" for_builder)
              "" for_builder
          in
          let for_body = L.append_block context "for_body" current_function in
          let body_builder = L.builder_at_end context for_body in
          let index = L.build_load counter "" body_builder in
          let _ =
            match ty with
            | List _ ->
                L.build_call getElement
                  [|
                    iterable;
                    index;
                    L.build_bitcast temp (L.pointer_type i8_t) "" body_builder;
                  |]
                  "" body_builder
            | String ->
                L.build_store
                  (L.build_call getChar [| iterable; index |] "" body_builder)
                  temp body_builder
            | _ -> raise Invalid
          in
          let flatten_body = function
            | SBlock lst -> build_stmt_list new_table body_builder lst
            | _ -> raise Invalid
          in
          let updated_builder = flatten_body stmt in
          let add_res =
            L.build_add
              (L.build_load counter "" updated_builder)
              (L.const_int i32_t 1) "" updated_builder
          in
          ignore (L.build_store add_res counter updated_builder);
          add_terminal updated_builder start_for;

          let for_end = L.append_block context "for_end" current_function in

          ignore (L.build_cond_br llvalue for_body for_end for_builder);

          (L.builder_at_end context for_end, table)
      | SRange (name, sexpr1, sexpr2, stmt) ->
          let e1' = build_expr table builder sexpr1 in
          let hi = build_expr table builder sexpr2 in

          (* Allocate temporary variable to store *)
          let low = L.build_alloca (lltype_of_type A.Int) name builder in
          ignore (L.build_store e1' low builder);

          let new_table = St.add_scope table in
          let new_table = St.add_current_scope new_table name (low, A.Int) in

          let for_loop = L.append_block context "for" current_function in

          let start_for = L.build_br for_loop in
          (* partial function *)
          ignore (start_for builder);

          let for_builder = L.builder_at_end context for_loop in

          let llvalue =
            L.build_icmp L.Icmp.Slt
              (L.build_load low "" for_builder)
              hi "" for_builder
          in
          let for_body = L.append_block context "for_body" current_function in
          let body_builder = L.builder_at_end context for_body in
          let flatten_body = function
            | SBlock lst -> build_stmt_list new_table body_builder lst
            | _ -> raise Invalid
          in
          let updated_builder = flatten_body stmt in
          let add_res =
            L.build_add
              (L.build_load low "" updated_builder)
              (L.const_int i32_t 1) "" updated_builder
          in
          ignore (L.build_store add_res low updated_builder);
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

          let start_while = L.build_br while_begin in
          (* partial function *)
          ignore (start_while builder);

          let while_builder = L.builder_at_end context while_begin in

          let llvalue = build_expr table while_builder sexpr in
          let while_body =
            L.append_block context "while_body" current_function
          in
          let updated_builder, _ =
            build_stmt table (L.builder_at_end context while_body) stmt
          in
          add_terminal updated_builder start_while;

          let while_end = L.append_block context "while_end" current_function in

          ignore (L.build_cond_br llvalue while_body while_end while_builder);
          (L.builder_at_end context while_end, table)
      | SReturn sexpr ->
          let ty = fst sexpr in
          let _ =
            match ty with
            | Void -> L.build_ret_void builder
            | _ ->
                let e = build_expr table builder sexpr in
                L.build_ret e builder
          in
          (builder, table)
      | _ ->
          ignore (Printf.printf "irgen stmt");
          raise Unimplemented
    in

    let funcbuilder =
      match fdecl.sbody with
      | SBlock lst ->
          build_stmt_list symbol_table builder lst (* Flatten sstmt *)
      | _ -> raise Invalid
    in

    if fdecl.sreturn_type = Void then add_terminal funcbuilder L.build_ret_void
  in

  let build_struct struct_ =
    let struct_name = struct_.sstruct_name in
    let struct_type = A.Struct struct_name in
    let struct_methods =
      List.map
        (fun method_ ->
          {
            sfunc_name = struct_name ^ "_" ^ method_.sfunc_name;
            sparameters = [ (struct_type, "this") ] @ method_.sparameters;
            sreturn_type = method_.sreturn_type;
            sbody = method_.sbody;
          })
        struct_.smethods
    in
    let my_fields = StringMap.find struct_name struct_fields in
    let lookup_identifier name table builder =
      try St.lookup_identifier name table
      with e ->
        let struct_, _ = St.lookup_identifier "this" table in
        let index, ty = StringMap.find name my_fields in
        let llv =
          L.build_struct_gep (L.build_load struct_ "" builder) index "" builder
        in
        (llv, ty)
    in
    List.iter (build_function_body lookup_identifier) struct_methods
  in

  List.iter build_struct structs;
  List.iter
    (build_function_body (fun name table _ -> St.lookup_identifier name table))
    functions;
  the_module
