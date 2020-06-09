open Ast
open Sast
open Exceptions
open Print_ast
open Helper


(* Map used for symbol table *)
module StringMap = Map.Make(String)
module Set = Set.Make(String)
module St = Symbol_table

let check (globals, functions, structs) =
  (* let add_identifier map (ty, str) = 
    StringMap.add str ty map
  in *)


  (* Check for duplicate variables *)
  let check_dups variables =
    let rec dups = function
        [] -> ()
      | ((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure (n1 ^ " declared more than once"))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) variables)
  in

  check_dups globals;


  (* Symbol table which consists of global variables to start *)
  let symbol_table = 
    List.fold_left (fun accum (type_, name) -> St.add_current_scope accum name type_) (St.add_scope (St.empty ())) globals in


  (* Add all functions to map to create symbol table *)
  let function_decls : Ast.func_def StringMap.t = List.fold_left add_func StringMap.empty (functions @ built_in_functions) in

  (* All struct functions *)
  let struct_functions : Ast.func_def StringMap.t StringMap.t = 
    let add_methods map _struct = 
      let methods = List.fold_left (fun map func -> StringMap.add func.func_name func map) StringMap.empty _struct.methods in
      StringMap.add _struct.struct_name methods map
    in
    List.fold_left add_methods StringMap.empty structs
  in

  (* All struct fields *)
  let struct_fields : Ast.ty StringMap.t StringMap.t = 
    let add_fields map _struct = 
      let fields = List.fold_left (fun map bind -> StringMap.add (snd bind) (fst bind) map) StringMap.empty _struct.fields in
      StringMap.add _struct.struct_name fields map
    in
    List.fold_left add_fields StringMap.empty structs
  in

  let find_struct_value map struct_name value = 
    try
      let values = StringMap.find struct_name map in
      StringMap.find value values
    with Not_found -> raise Invalid
  in

  let _ = 
    try 
      let main_func = find_func function_decls "main" in
      if main_func.return_type <> Void then raise(InvalidReturnType("main() should have void return type"))
    with FunctionDoesntExist _ -> raise MainEntrypointUndefined
  in

  let rec check_func symbol_table func = 

    let vars_outside_scope = ref Set.empty in
    let scope_count = ref 0 in
    let heap_vars = ref Set.empty in

    check_dups func.parameters;

    (* Initial symbol table only contains globally defined variables *)
    let symbol_table = 
      List.fold_left (fun accum (type_, name) -> St.add_current_scope accum name type_) (St.add_scope symbol_table) func.parameters in

    let rec check_expr table = function
      | IntLit integer -> (Int, SIntLit integer)
      | BoolLit boolean -> (Bool, SBoolLit boolean)
      | FloatLit fl -> (Float, SFloatLit fl)
      | CharLit ch -> (Char, SCharLit ch)
      | StringLit str -> (String, SStringLit str)
      | FunctionLit lambda -> 
        let new_table = St.add_scope table in
        let (sfunc, vars) = check_func new_table lambda in
        (* Set.iter (fun k -> Printf.printf "Set %s\n" k) vars; *)
        let vars = Set.fold (fun k lst -> k :: lst) vars [] in
        (* let infer_heap_vars  = List.fold_left (fun lst el -> if StringMap.mem el (List.hd table) then el :: lst else lst) [] vars in
        heap_vars := List.fold_left (fun heapvars el -> Set.add el heapvars) !heap_vars infer_heap_vars; *)
        let types = List.map (fun (ty, _) -> ty) sfunc.sparameters in
        (Function (types, sfunc.sreturn_type), SFunctionLit (vars, sfunc))
      | TypeLit l -> (Type l, STypeLit l)
      | Seq lst -> 
        let new_list = List.map (check_expr table) lst in

        (* Helper function used to the type of a list *)
        let rec infer_type lst = 
          if List.length lst = 1 then (
            fst (List.hd lst)
          ) else (
            match lst with
            | [] -> raise Seq_type_error
            | hd :: tl -> 
              let (ty, _) = hd in 
              let ty_rest = infer_type tl in
              if ty = ty_rest then ty 
              else raise (TypeMismatch("Can't have sequence containing " ^ (string_of_typ ty) ^ " and " ^ (string_of_typ ty_rest)))
          )
        in

        (List (infer_type new_list), SSeq new_list)
      | Id name -> 
        let ty = St.lookup_identifier name table in
        (* let _ = if in_function_scope !scope_count name table then (
            ()
          ) else (
            vars_outside_scope := Set.add name !vars_outside_scope
          )
        in *)
        (ty, SId name)
      | Binop (lhs, op, rhs) ->
        let (t1, lhs') = check_expr table lhs in
        let (t2, rhs') = check_expr table rhs in
        if t1 = t2 then (
          let ty = match op with
            (* + - * / *)
              Add ->
              begin match t1 with
                  Int -> Int
                | Float -> Float
                | Char -> Char
                | String -> String
                | _ -> raise Bad_arithmetic
              end
            | Sub | Mult | Div -> 
              begin match t1 with
                  Int -> Int
                | Float -> Float
                | Char -> Char
                | _ -> raise Bad_arithmetic
              end
            (* % *)
            | Mod -> 
              begin match t1 with
                  Int -> Int
                | _ -> raise Unimplemented
              end
            (* && || *)
            | And | Or ->
              begin match t1 with
                  Bool -> Bool
                | _ -> raise Bad_logical
              end
            (* ==, != *)
            | Equal | Neq ->
              begin match t1 with
                (* Only only equality comparison for basic types for now *)
                  Int | Float | Char | Bool -> Bool
                | _ -> raise Bad_equal
              end
            (* <, >, <=, >= *)
            | Less | Greater | Leq | Geq -> 
              begin match t1 with
                (* Should only be able to do these compare for integers, floats, and characters *)
                | Int | Float | Char -> Bool
                | _ -> raise Bad_compare
              end
          in
          (ty, SBinop((t1,lhs'), op, (t2, rhs')))
        ) 
        else (
          raise (IllegalBinOp (Printf.sprintf "Illegal operation"))
        )
      | Unop(op, e) as ex -> 
        let (t, e') = check_expr table e in
        let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (IllegalUnOp ("Illegal unary operator " ^
                                     string_of_uop op ^ string_of_typ t ^
                                     " in " ^ string_of_expr ex))
        in (ty, SUop(op, (t, e')))
      | Call (name, arguments) as call -> 
        let (name, parameters, return_type) = 
          if func_exists function_decls name then (
            let f = find_func function_decls name in
            let params = List.map (fun (ty, _) -> ty) f.parameters in
            (f.func_name, params, f.return_type)
          ) else (
            let func =   
              begin match St.lookup_identifier name table with
                  Function(func) -> func
                | _ -> raise Invalid
              end
            in
            (name, fst func, snd func)
          )
        in
        let len = List.length parameters in
        if List.length arguments <> len then (
          raise (Failure ("expecting " ^ string_of_int len ^
                          " arguments in " ^ string_of_expr call))
        ) else (
          let check_call ft e =
            let (ty, e') = check_expr table e in
            let err = "illegal argument found " ^ string_of_typ ty ^
                      " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e in
            (check_assign ft ty err, e')
          in
          if name = "append" then (

            let (ty, e1) = check_expr table (List.hd arguments) in
            let (el_ty, e2) =  check_expr table (List.nth arguments 1) in
            begin match ty with
                List rest -> 
                if rest = el_ty then (
                  (return_type, SCall(name, [(ty, e1); (el_ty, e2)]))
                ) else (
                  raise Func_failed_typecheck
                )
              | _ -> raise (IncorrectArgumentType("Expected a list as first argument to append"))
            end

          ) else if name = "len" then (
            let (ty, e1) = check_expr table (List.hd arguments) in
            begin match ty with 
                List _  | String -> (return_type, SCall(name, [(ty, e1)]))
              | _ -> raise Func_failed_typecheck
            end
          ) else if name = "pop" then (
            let (ty, e1) = check_expr table (List.hd arguments) in
            begin match ty with 
                List _ -> (return_type, SCall(name, [(ty, e1)])) 
              | _ -> raise Func_failed_typecheck
            end
          ) else if name = "put" then (
            let sargs = List.map (check_expr table) arguments in
            let (ty, e1) = List.hd sargs in
            begin match ty with
              | List rest -> 
                if rest = fst (List.nth sargs 2) && fst (List.nth sargs 1) = Int then (
                  return_type, SCall(name, sargs)
                )
                else (
                  raise Func_failed_typecheck
                )
              | _ -> raise Func_failed_typecheck
            end
          ) else if name = "map" then (

            let (ty, e1) = check_expr table (List.hd arguments) in
            let (el_ty, e2) =  check_expr table (List.nth arguments 1) in
            begin match ty with
                List rest -> 
                begin match el_ty with
                    Function ([input], out) ->
                    if rest <> input then (
                      raise (IncorrectArgumentType("Input to function should be same as list type"))
                    )
                    else (
                      (List out, SCall(name, [(ty, e1); (el_ty, e2)]))
                    )
                  | _ -> raise (IncorrectArgumentType("Input to map should be a list and function accepting list element"))
                end
              | _ -> raise (IncorrectArgumentType("Expected a list as first argument to append"))
            end

          ) else if name = "println" then (
            let (ty, e) = check_expr table (List.hd arguments) in
            match ty with
            | Int | Char | Bool | Float | String -> (return_type, SCall(name, [(ty, e)]))
            | _ -> raise (IllegalArgument("Can't print " ^ string_of_typ ty))
          ) else if name = "make" then (
            let sargs = List.map (check_expr table) arguments in
            let (ty, _) = List.hd sargs in
            match ty with
            | Type el_ty -> 
              let (arg2_ty, _) = List.nth sargs 1 in
              let (arg3_ty, _) = List.nth sargs 2 in
              if arg2_ty = Int && arg3_ty = el_ty then (List el_ty, SCall(name, sargs))
              else raise (IncorrectArgumentType("Incorrect arguments supplied to make()"))
            | _ -> raise (IncorrectArgumentType("Expected type of list elements as first argument"))
          ) else (
            let sargs = List.map2 check_call parameters arguments in 
            (return_type, SCall(name, sargs))
          )
        )
      | SeqAccess (var, inside_bracket) -> 
        let (ty, e') = check_expr table inside_bracket in
        let (type_, e1) = check_expr table var in
        begin match ty with
          | Int -> 
            begin match type_ with
              | String -> (Char, SSeqAccess((type_, e1), (ty, e')))
              | List ty -> (ty, SSeqAccess((type_, e1), (ty, e')))
              | _ -> raise (IllegalAccess ((string_of_typ type_) ^ " is not a sequential type"))
            end
          | _ -> raise (IllegalAccess ("Can't access sequential type with " ^ string_of_typ ty))
        end
      | StructCall(var, func, args) as struct_call -> 
        let ty = St.lookup_identifier var table in
        let name = struct_name ty in
        let struct_func = find_struct_value struct_functions name func in
        let len = List.length struct_func.parameters in
        if List.length args <> len then (
          raise (Failure ("expecting " ^ string_of_int len ^
                          " arguments in " ^ string_of_expr struct_call))
        ) else (
          let check_call (ft, _) e =
            let (ty, e') = check_expr table e in
            let err = "illegal argument found " ^ string_of_typ ty ^
                      " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e in
            (check_assign ft ty err, e')
          in
          let sargs = List.map2 check_call struct_func.parameters args in
          (struct_func.return_type, SStructCall(var, func, sargs))
        )
      | StructAccess(var, field) -> 
        let ty = St.lookup_identifier var table in
        let name = struct_name ty in
        let field_ty = find_struct_value struct_fields name field in
        (field_ty, SStructAccess(var, field))
      | _ -> raise Unimplemented (* Ignore for now *)
    and check_bool_expr table expr = 
      let (ty, e') = check_expr table expr in
      (* type of this expression must be a boolean *)
      match ty with
      | Bool -> (ty, e')
      | _ -> raise Invalid (* Can come up with a better error message *)
    and check_stmt_list table = function
        [] -> ([], table)
      | head :: tail -> 
        let (sast, table) = check_stmt table head in
        let (sastlst, table) = check_stmt_list table tail in
        (sast::sastlst, table)
    and check_stmt table = function 
      | Return e -> 

        let (ty, e') = check_expr table e in
        let ty = match ty with Type t -> t | _ -> ty in
        if ty = func.return_type then (SReturn(ty, e'), table)
        else raise (InvalidReturnType (Printf.sprintf "Returned %s must be of type %s" (string_of_expr e) (string_of_typ ty)))

      | If (expr, stmt) -> (SIf(check_bool_expr table expr, fst (check_stmt table stmt)), table)
      | Block lst -> 

        let new_table = St.add_scope table in 
        scope_count := !scope_count + 1;
        let sblock = fst (check_stmt_list new_table lst) in
        scope_count := !scope_count - 1;
        (SBlock(sblock), table)

      | Expr expr -> (SExpr(check_expr table expr), table)
      | Declare (ty, name) -> 

        let table = St.add_current_scope table name ty in 
        (SDeclare(ty, name), table)

      | Explicit ((ty, name),expr)->

        let (expr_ty, e') = check_expr table expr in
        if expr_ty = ty then (
          let table = St.add_current_scope table name expr_ty in 
          (SExplicit((ty, name), (expr_ty, e')), table)
        ) else raise InvalidAssignment

      | Assign (name, e) -> 

        let (ty, e') = check_expr table e in
        let vartype = St.lookup_identifier name table in
        let ty = check_assign vartype ty "" in
        (SAssign(name, (ty, e')), table)

      | AssignSeq(seq, index, var) ->
        
        let (ty, e') = check_expr table seq in 
        let (ind_ty, sindex) = check_expr table index in
        let _ = if ind_ty <> Int then raise (IllegalAccess ("Can't access sequential type with " ^ string_of_typ ty)) in

        let (vartype, svar) = check_expr table var in

        let el_ty = get_element_type ty in

        if el_ty = vartype then (SAssignSeq((ty, e'), (ind_ty, sindex), (vartype,svar)), table)
        else raise InvalidAssignment

      | Define (name, expr) -> 

        let (expr_ty, e') = check_expr table expr in
        let table = St.add_current_scope table name expr_ty in 
        (SDefine(name, (expr_ty, e')), table)

      | IfElse (expr, stmt1, stmt2) -> (SIfElse(check_bool_expr table expr, fst (check_stmt table stmt1), fst (check_stmt table stmt2)), table)
      | Iterate (x, e, stmt) ->

        let (ty, e') = check_expr table e in
        let get_siterate el_ty = function
            Block lst -> (* flatten list *)
            let new_table = St.add_current_scope (St.add_scope table) x el_ty in 
            scope_count := !scope_count + 1;
            let sblock = fst (check_stmt_list new_table lst) in
            scope_count := !scope_count - 1;
            (SIterate(x, (ty, e'), SBlock(sblock)), table)
          | _ -> raise Invalid
        in

        begin match ty with
          | List el -> get_siterate el stmt
          | String -> get_siterate Char stmt
          | _ -> raise Invalid
        end

      | Range(x, e1, e2, stmt) ->

        let (t1, e1') = check_expr table e1 in
        let (t2, e2') = check_expr table e2 in
        let get_srange = function
            Block lst -> (* flatten list *)
            let new_table = St.add_current_scope (St.add_scope table) x Int in
            scope_count := !scope_count + 1;
            let sblock = fst (check_stmt_list new_table lst) in
            scope_count := !scope_count - 1; 
            (SRange(x, (t1, e1'), (t2, e2'), SBlock(sblock)), table)
          | _ -> raise Invalid
        in

        begin match t1, t2 with
          | Int, Int -> get_srange stmt
          | _, _ -> raise Invalid
        end

      | While (e, stmt) -> (SWhile(check_bool_expr table e, fst (check_stmt table stmt)), table)
      | _ -> raise Unimplemented (* Ignore for now *)
    in


    let (sast, table) = 
      match func.body with
        Block lst -> check_stmt_list symbol_table lst (* flatten block to list *)
      | _ ->  raise Invalid
    in

    (* StringMap.iter (fun k v -> Printf.printf "Map %s -> %s\n" k (string_of_typ v)) (List.hd table); *)
    Set.iter (fun k -> Printf.printf "Heap vars %s\n" k) !heap_vars;
    let sast_func = SBlock(sast) in

    ({
      sfunc_name = func.func_name;
      sparameters = func.parameters;
      sreturn_type = func.return_type;
      sbody = sast_func;
      sheap_vars = Set.fold (fun k lst -> k :: lst) !heap_vars []
    }, !vars_outside_scope)
  in
  let check_struct _struct =
    let symbol_table = List.fold_left (fun st (ty, name) -> St.add_current_scope st name ty) (St.add_scope symbol_table) _struct.fields in
    (* Create symbol table with struct vars then check_func for each of the methods *)
    {
      sstruct_name = _struct.struct_name;
      sfields = _struct.fields;
      smethods = List.map (fun func -> fst (check_func symbol_table func)) _struct.methods
    }
  in
  let get_func func =
    let (func, _) = check_func symbol_table func in
    func
  in
  (globals, List.map get_func functions, List.map check_struct structs)