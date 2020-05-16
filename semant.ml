open Ast
open Sast
open Exceptions
open Printing


(* Map used for symbol table *)
module StringMap = Map.Make(String)


let check (globals, functions, structs) =
  let add_identifier map (ty, str) = 
    StringMap.add str ty map
  in
  
  let check_dups variables =
    let rec dups = function
        [] -> ()
      | ((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure (n1 ^ " declared more than once"))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) variables)
  in
  
  check_dups globals;

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

  let built_in_functions = [
    {
      return_type = Int;
      func_name = "printi";
      parameters = [(Int, "x")];
      body = Block []
    };
    {
      return_type = Int;
      func_name = "printc";
      parameters = [(Char, "x")];
      body = Block []
    };
    {
      return_type = Int;
      func_name = "printfl";
      parameters = [(Float, "x")];
      body = Block []
    };
    {
      return_type = Int;
      func_name = "prints";
      parameters = [(String, "x")];
      body = Block []
    };
    {
      return_type = Int;
      func_name = "printb";
      parameters = [(Bool, "x")];
      body = Block []
    };
    {
      return_type = Int;
      func_name = "lenstr";
      parameters = [(String, "x")];
      body = Block []
    };
    {
      return_type = Int;
      func_name = "append";
      parameters = [(List AnyType, "x"); (AnyType, "y")];
      body = Block []
    };
    {
      return_type = Int;
      func_name = "put";
      parameters = [(List AnyType, "x"); (Int, "y") ;(AnyType, "z")];
      body = Block []
    };
    {
      return_type = Int;
      func_name = "lenlist";
      parameters = [(List AnyType, "x")];
      body = Block []
    };
    {
      return_type = Int;
      func_name = "pop";
      parameters = [(List AnyType, "x")];
      body = Block []
    }
  ]
  in

  (* Add all functions to map to create symbol table --> NOTE: May need to change empty map to built_in_functions *)
  let function_decls = List.fold_left add_func StringMap.empty (functions @ built_in_functions)
  in
  
  let _ = 
    try 
      find_func function_decls "main"
    with FunctionDoesntExist -> raise MainEntrypointUndefined
  in

  let check_func func = 

    check_dups func.parameters;

    let parameters = List.fold_left add_identifier StringMap.empty func.parameters in

    (* Initial symbol table only contains globally defined variables *)
    let symbol_table = [parameters; globalvars] in

    (* Adds a new (current) scope to symbol_table list to the beginning of the list *)
    let add_scope table = StringMap.empty :: table
    in

    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Add the variable to the current scope. 
    Takes in symbol_table, name of identifier, and type of identifier *)
    let add_to_current_scope table name ty =
      if StringMap.mem name (List.hd table) then raise VariableAlreadyExists
      else List.mapi (fun idx map -> if idx = 0 then StringMap.add name ty map else map) table
    in

    (* Looks for identifier starting from current scope --> global scope. Takes in name of identifier and symbol_table *)
    let rec lookup_identifier name = function
      | [] -> raise (UnrecognizedIdentifier (Printf.sprintf "%s: Unrecognized Identifier" name))
      | current_scope :: tl -> 
        try
          StringMap.find name current_scope
        with Not_found ->
          lookup_identifier name tl
    in

    let rec check_expr table = function 
    (* Todo: Nonetype *)
      | IntLit l -> (Int, SIntLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | CharLit l -> (Char, SCharLit l)
      | StringLit l -> (String, SStringLit l)
      | Seq lst -> 
          let new_list = List.map (check_expr table) lst in
          let rec infer_type lst = 
            if List.length lst == 1 then fst (List.hd lst)
            else (match lst with
            | [] -> raise Seq_type_error
            | hd :: tl -> let (ty, _) = hd in if ty = infer_type tl then ty else raise Seq_type_error)
          in
          (List (infer_type new_list), SSeq new_list)
      | Id name -> (lookup_identifier name table, SId name)
      | Binop (lhs, op, rhs) ->
        let (t1, lhs') = check_expr table lhs in
        let (t2, rhs') = check_expr table rhs in
        if t1 = t2 then
          let ty = match op with
          | Add | Sub | Mult | Div -> 
            (* Should only be able to perform these operations with integers, floats, and chars *)
            (match t1 with
            | Int -> Int
            | Float -> Float
            | Char -> Char
            | _ -> raise Bad_arithmetic)
          | Mod -> (match t1 with
            | Int -> Int
            | _ -> raise Unimplemented)
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
        else raise (IllegalBinOp (Printf.sprintf "Illegal operation"))
      
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
        let f = find_func function_decls name in
        let len = List.length f.parameters in
        if List.length arguments != len then
          raise (Failure ("expecting " ^ string_of_int len ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
          let (ty, e') = check_expr table e in
          let err = "illegal argument found " ^ string_of_typ ty ^
          " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e in
          (check_assign ft ty err, e')
        in
        if name = "append" then (* a special function which needs special care :) *)
            let (ty, e1) = check_expr table (List.hd arguments) in
            let (el_ty, e2) =  check_expr table (List.nth arguments 1) in
            (match ty with
            List rest -> if rest = el_ty then (f.return_type, SCall(name, [(ty, e1); (el_ty, e2)])) else raise Func_failed_typecheck
            | _ -> raise (IncorrectArgumentType("Expected a list as first argument to append")))
        else if name = "lenlist" || name = "pop" then
        let (ty, e1) = check_expr table (List.hd arguments) in
          (match ty with List _ -> (f.return_type, SCall(name, [(ty, e1)])) | _ -> raise Func_failed_typecheck)
        else if name = "put" then
          let sargs = List.map (check_expr table) arguments in
          let (ty, e1) = List.hd sargs in
          (
          match ty with
          | List rest -> 
            if rest = fst (List.nth sargs 2) && fst (List.nth sargs 1) = Int then (f.return_type, SCall(name, sargs))
            else raise Func_failed_typecheck
          | _ -> raise Func_failed_typecheck
          )
        else let sargs = List.map2 check_call f.parameters arguments
        in (f.return_type, SCall(name, sargs))
      | SeqAccess (var, inside_bracket) -> 
        let (ty, e') = check_expr table inside_bracket in
        let (type_, e1) = check_expr table var in
        (
        match ty with
        | Int -> (
          match type_ with
          | String -> (Char, SSeqAccess((type_, e1), (ty, e')))
          | List ty -> (ty, SSeqAccess((type_, e1), (ty, e')))
          | _ -> raise (IllegalAccess ((string_of_typ type_) ^ " is not a sequential type"))
          )
        | _ -> raise (IllegalAccess ("Can't access sequential type with " ^ string_of_typ ty))
        )
      | _ -> raise Unimplemented (* Ignore for now *)
    in

    let check_bool_expr table expr = 
      let (ty, e') = check_expr table expr in
      (* type of this expression must be a boolean *)
      match ty with
      | Bool -> (ty, e')
      | _ -> raise Invalid (* Can come up with a better error message *)
    in


    let rec check_stmt_list table = function
        [] -> []
        | head :: tail -> 
          let (sast, table) = check_stmt table head in
            sast :: check_stmt_list table tail
    and check_stmt table = function 
      | Return e -> 
        let (ty, e') = check_expr table e in
        if ty = func.return_type then (SReturn(ty, e'), table)
        else raise (InvalidReturnType (Printf.sprintf "Returned %s must be of type %s" (string_of_expr e) (string_of_typ ty)))
      | If (expr, stmt) -> (SIf(check_bool_expr table expr, fst (check_stmt table stmt)), table)
      | Block lst -> 
        let new_table = add_scope table in 
        (SBlock(check_stmt_list new_table lst), table)
      | Expr expr -> (SExpr(check_expr table expr), table)
      | Declare (ty, name) -> 
        let table = add_to_current_scope table name ty in 
          (SDeclare(ty, name), table)
      | Explicit ((ty, name),expr)->
        let (expr_ty, e') = check_expr table expr in
        if expr_ty = ty then (
          let table = add_to_current_scope table name expr_ty in 
          (SExplicit((ty, name), (expr_ty, e')), table)
        )
        else (
          raise InvalidAssignment
        )
      | Assign (name, e) -> 
        let (ty, e') = check_expr table e in
        let vartype = lookup_identifier name table in
        let ty = check_assign vartype ty "" in
        (SAssign(name, (ty, e')), table)
      | Define (name, expr) -> 
        let (expr_ty, e') = check_expr table expr in
        let table = add_to_current_scope table name expr_ty in 
        (SDefine(name, (expr_ty, e')), table)
      | IfElse (expr, stmt1, stmt2) -> (SIfElse(check_bool_expr table expr, fst (check_stmt table stmt1), fst (check_stmt table stmt2)), table)
      | Iterate (x, e, stmt) ->
        let (ty, e') = check_expr table e in
        let get_siterate el_ty = (function
          Block lst -> (* flatten list *)
            let new_table = add_to_current_scope (add_scope table) x el_ty in 
            (SIterate(x, (ty, e'), SBlock(check_stmt_list new_table lst)), table)
          | _ -> raise Invalid)
        in
        (match ty with
        | List el -> get_siterate el stmt
        | String -> get_siterate Char stmt
        | _ -> raise Invalid)
      | While (e, stmt) -> (SWhile(check_bool_expr table e, fst (check_stmt table stmt)), table)
      | _ -> raise Unimplemented (* Ignore for now *)
    in
    let sast_func = SBlock (match func.body with
      Block lst -> check_stmt_list symbol_table lst (* flatten block to list *)
      | _ ->  raise Invalid
    ) in
    {
      sfunc_name = func.func_name;
      sparameters = func.parameters;
      sreturn_type = func.return_type;
      sbody = sast_func
    }
  in
  let check_struct stuct_ = ignore(print_endline "check_struct"); raise Unimplemented (* ignore for now *) in
  (globals, List.map check_func functions, List.map check_struct structs)