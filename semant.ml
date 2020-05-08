open Ast
open Sast
open Exceptions


(* Map used for symbol table *)
module StringMap = Map.Make(String)


let check (globals, functions, structs) =
  let add_identifier map (ty, str) = 
    StringMap.add str ty map
  in

  
  let check_dups =
    let rec dups = function
        [] -> ()
      | ((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure (n1 ^ " declared more than once"))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) globals)
  in

  let globalvars = List.fold_left add_identifier StringMap.empty globals in
  
  let add_func map f = 
    (* Check to see if name already exists --> NOTE: May need to add more for built in function *)
    if StringMap.mem f.func_name map then raise FunctionAlreadyExists
    else StringMap.add f.func_name f map
  in

  let find_func map name = 
    try
      StringMap.find name map
    with Not_found -> raise FunctionDoesntExist
  in

  (* Add all functions to map to create symbol table --> NOTE: May need to change empty map to built_in_functions *)
  let function_decls = List.fold_left add_func StringMap.empty functions 
  in 

  let check_func func = 

    (* Initial symbol table only contains globally defined variables *)
    let symbol_table = [globalvars] in

    (* Adds a new (current) scope to symbol_table list to the beginning of the list *)
    let add_scope table = StringMap.empty :: table
    in

    (* Remove scope once you exit a block *)
    let remove_scope table = List.tl table
    in

    (* Add the variable to the current scope. Takes in symbol_table, name of identifier, and type of identifier *)
    let add_to_current_scope table name ty =
      List.mapi (fun idx map -> if idx = 0 then StringMap.add name ty map else map) table
    in

    (* Looks for identifier starting from current scope --> global scope. Takes in name of identifier and symbol_table *)
    let rec lookup_identifier name = function
      | [] -> raise (UnrecognizedIdentifier "TODO: ERROR MESSAGE")
      | current_scope :: tl -> 
        try
          StringMap.find name current_scope
        with Not_found ->
          lookup_identifier name tl
    in

    let rec check_expr = function 
      | IntLit l -> (Int, SIntLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | CharLit l -> (Char, SCharLit l)
      | StringLit l -> (String, SStringLit l)
      | Seq lst -> raise Unimplemented (* Ignore for now *)
      | Id name -> (lookup_identifier name symbol_table, SId name)
      | Binop (lhs, op, rhs) ->
        let (t1, lhs') = check_expr lhs in
        let (t2, rhs') = check_expr rhs in
        if t1 = t2 then
          let ty = match op with
          | Add | Sub | Mult | Div -> 
            (* Should only be able to perform these operations with integers, floats, and chars *)
            (match t1 with
            | Int -> Int
            | Float -> Float
            | Char -> Char
            | _ -> raise Bad_arithmetic)
          | And | Or ->
            (match t1 with
            | Bool -> Bool
            | _ -> raise Bad_logical)
          | Equal | Neq ->
            (match t1 with
            (* Only only equality comparison for basic types for now *)
            | Int | Float | Char | Bool -> Bool
            | _ -> raise Bad_equal)
          | Less | Greater | Leq | Geq -> 
            (match t1 with
            (* Should only be able to do these compare for integers, floats, and characters *)
            | Int | Float | Char -> Bool
            | _ -> raise Bad_compare)
          in
          (ty, SBinop((t1,lhs'), op, (t2, rhs')))
        else raise (IllegalBinOp "TODO: INSERT EXPRESSION")
      
      | Assign (name, e) -> 
        let (ty, e') = check_expr e in
        (* TODO: Need to make sure that variable already exists in symbol table *)
        (ty, SAssign(name, (ty, e')))
      (* | Call (name, arguments) -> 
        (* TODO: Make sure that function is declared in find_func *)
        let f = find_func function_decls name in
        let len = List.length arguments in
        if len == List.length f.parameters then
            (* TODO: check_call function *)
            let sargs = List.map2 check_call arguments f.parameters in
            let ty = f.return_type in
            (ty, SCall(name, sargs))
        else raise WrongNumberOfArugments *)
      | SeqAccess (var_name, inside_bracket) -> raise Unimplemented (* Ignore for now *)
      | StructCall (var_name, method_name, args) -> raise Unimplemented (* Ignore for now *)
      | StructAccess (var_name, instance_var) -> raise Unimplemented (* Ignore for now *)
      | StructAssign (var_name, instance_var, expr) -> raise Unimplemented (* Ignore for now *)
      | StructLit (struct_name, values) -> raise Unimplemented (* Ignore for now *)
      | _ -> raise Unimplemented (* Ignore for now *)
    in

    let check_bool_expr expr = 
      let (ty, e') = check_expr expr in
      (* type of this expression must be a boolean *)
      match ty with
      | Bool -> (ty, e')
      | _ -> raise Invalid (* Can come up with a better error message *)
    in


    let rec check_stmt_list = function
        [] -> []
        | head :: tail -> check_stmt head :: check_stmt_list tail
    and check_stmt = function 
      | Return e -> 
        let (ty, e') = check_expr e in
        if ty = func.return_type then SReturn(ty, e')
        else raise (InvalidReturnType "TODO: FILL IN INFO")
      | If (expr, stmt) -> SIf(check_bool_expr expr, check_stmt stmt)
      | Block lst -> SBlock(check_stmt_list lst)
      | Expr expr -> SExpr(check_expr expr)
      | Explicit ((ty, name),expr)-> (* TODO: Will need to add variable to symbol table *)
        let (expr_ty, e') = check_expr expr in
        if expr_ty = ty then SExplicit((ty, name), (expr_ty, e'))
        else raise InvalidAssignment
      | Define (name, expr) -> SDefine(name, check_expr expr) (* TODO: Will need to add variable to symbol table *)
      | IfElse (expr, stmt1, stmt2) -> SIfElse(check_bool_expr expr, check_stmt stmt1, check_stmt stmt2)
      | Iterate (x, e, stmt) ->
        let (ty, e') = check_expr e in
        (match ty with
        | List _ | String -> SIterate(x, (ty, e'), check_stmt stmt)
        | _ -> raise Invalid)
      | While (e, stmt) -> SWhile(check_bool_expr e, check_stmt stmt)
    in
    {
      sfunc_name = func.func_name;
      sparameters = func.parameters;
      sreturn_type = func.return_type;
      sbody = check_stmt_list func.body
    }
  in
  let check_struct stuct_ = raise Unimplemented (* ignore for now *) in
  (globals, List.map check_func functions, List.map check_struct structs)
  