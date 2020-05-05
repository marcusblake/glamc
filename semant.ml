open Ast
open Sast
open Exceptions


(* Map used for symbol table *)
module StringMap = Map.Make(String)


let type_signature (func_def: func_def)  = 
  Arrow(List.map fst func_def.parameters, func_def.return_type)

(* let lookup_in_context context str = 
  try StringMap.find str context 
  with Not_found -> raise (UnrecognizedIdentifier str)
;; *)

let lookup_identifier str = 
  try StringMap.find str symbols
  with Not_found -> raise (UnrecognizedIdentifier str)

let check (globals, functions, structs) =

  let add_func map f = 
    (* Check to see if name already exists --> NOTE: May need to add more for built in function *)
    if StringMap.mem f.name map then raise FunctionAlreadyExists
    else StringMap.add f.name f map
  in

  (* Add all functions to map to create symbol table --> NOTE: May need to change empty map to built_in_functions *)
  let function_decls = List.fold_left add_func StringMap.empty functions 
  in 

  let check_func func = 
    let rec check_expr = function 
      | IntLit l -> (Int, SIntLit l)
      | BoolLit l -> (Bool, SBoolLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | CharLit l -> (Char, SFloatLit l)
      | StringLit l -> (String, SStringLit l)
      | Seq lst -> raise Unimplemented (* Ignore for now *)
      | Id name -> raise Unimplemented (* Ignore for now *)
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
          | Neq | Less | Greater | Leq | Geq -> 
            (match t1 with
            (* Should only be able to do these compare for integers, floats, and characters *)
            | Int | Float | Char -> Bool
            | _ -> raise Bad_compare)
          in
          (ty, SBinop((t1,lhs'), op, (t2, rhs'))
        else raise (IllegalBinOp "TODO: INSERT EXPRESSION")
      
      | Assign (name, e) -> 
        let (ty, e') = check_expr context e in
        (* TODO: Need to make sure that variable already exists in symbol table *)
        (ty, SAssign(name, e'))
      | Call (name, arguments) -> 
        (* TODO: Make sure that function is declared in find_func *)
        let f = find_func name in
        let len = List.length arguments in
        if len == List.length f.parameters then
            (* TODO: check_call function *)
            let sargs = List.map2 check_call arguments f.parameters in
            let ty = f.return_type in
            (ty, SCall(name, sargs))
        else raise WrongNumberOfArugments
      | SeqAccess (var_name, inside_bracket) -> raise Unimplemented (* Ignore for now *)
      | StructCall (var_name, method_name, args) -> raise Unimplemented (* Ignore for now *)
      | StructAccess (var_name, instance_var) -> raise Unimplemented (* Ignore for now *)
      | StructAssign (var_name, instance_var, expr) -> raise Unimplemented (* Ignore for now *)
      | StructLit (struct_name, values) -> raise Unimplemented (* Ignore for now *)
    in

    let check_bool_expr expr = 
      let (ty, e') = check_expr expr in
      (* type of this expression must be a boolean *)
      match ty with
      | Bool -> (ty, e')
      | _ -> raise Invalid (* Can come up with a better error message *)


    let rec check_stmt_list = function
        [] -> []
        | Block head :: tail -> check_stmt_list (head @ tail)
        | head :: tail -> check_stmt head :: check_stmt_list tail
    and check_stmt = function 
      | Return e -> 
        let (ty, e') = check_expr symbol_table e in
        if ty = f.return_type then SReturn(ty, e')
        else raise (InvalidReturnType "TODO: FILL IN INFO")
      | If (expr, stmt) -> SIf(check_bool_expr expr, check_stmt stmt)
      | Block lst -> SBlock(check_stmt_list lst)
      | Expr expr -> SExpr(check_expr expr)
      | Explicit ((ty, name),expr)->
        let (expr_ty, e') = check_expr expr in
        if expr_ty = ty then SExplicit((ty, name), (expr_ty, e'))
        else raise InvalidAssignment
      | Define (name, expr) -> SDefine(name, check_expr context expr)
      | IfElse (expr, stmt1, stmt2) -> SIfElse(check_bool_expr, check_stmt stmt1, check_stmt stmt2)
      | Iterate (x, e, stmt) ->
        let (ty, e') = check_expr e in
        match ty with
        | List _ | String -> SIterate(x, (ty, e'), check_stmt stmt)
        | _ -> false
      | While (e, stmt) -> SWhile(check_bool_expr expr, check_stmt stmt)
    in
  in


let check_func_def  (context : (ty * string) list) (f : func_def) : bool = 
  let parameters = f.parameters in
  let local_context = parameters @ context in
  let body = f.body in
  in 
    check_stmts context body
;;


let bind_of_func_def f =
  let name = f.func_name in
  let typ = type_signature f in
  (typ, name)
  ;;

let bind_of_struct_def sd = 
  let struct_name = sd.struct_name in
  let fields = sd.fields in
  let methods = sd.methods in
  let method_signatures = List.map bind_of_func_def methods in
  StructShape (struct_name, fields @ method_signatures), struct_name
;;
