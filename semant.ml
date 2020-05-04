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


let print_context ctx = 
   print_endline (String.concat ";" 
    (List.map (fun (ty, name) -> 
     name ^ ": " ^ (string_of_typ ty))
ctx
     )
     )
;;

let rec check_expr context expr = 
  match expr with
  | IntLit l -> (Int, SIntLit l)
  | BoolLit l -> (Bool, SBoolLit l)
  | FloatLit l -> (Float, SFloatLit l)
  | CharLit l -> (Char, SFloatLit l)
  | StringLit l -> (String, SStringLit l)
  (* | Seq lst -> 
    begin
      match lst with
      | head :: tail  -> 
          let expected = check_expr context head in
          let rec helper lst = match lst with
              | _ -> []
              | x :: xs -> 
                let t = check_expr context x in
                if t <> expected then raise Seq_type_error
                else helper xs
          in
            begin
              helper tail;
              List(expected)
            end
      | _ -> raise Empty_sequence
    end  *)
  | Id name -> 
      print_endline "looking up id";
      let ret = 
        if name = "print" || name = "println"
        then Arrow([String], Int)
        else lookup_in_context context name
      in
        print_endline "finished looking up id";
        ret
  | Binop (lhs, op, rhs) ->
    let (t1, lhs') = check_expr context lhs in
    let (t2, rhs') = check_expr context rhs in
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
    check_expr context e
  | Call (name, arguments) -> 
    print_endline "calling";
    print_endline name;
    if name = "print" || name = "println" 
    then Int
    else
    begin
      let funcType = 
  
        if name = "int_to_float" 
        then Arrow ([Int], Float)
        else if name = "int_to_string" 
        then Arrow([Int], String)
        else lookup_in_context context name 
         
      in
        
      match funcType with
          | Arrow (param_types, return_type) ->
              let rec helper arguments param_types = 
                match arguments, param_types with
                | arg :: args, ptype :: ptypes ->
                  let arg_type = check_expr context arg in
                  if typ_eq arg_type ptype 
                  then helper args ptypes
                  else 
                  begin
                    print_endline (string_of_typ arg_type);
                    print_endline (string_of_typ ptype);
                    raise Arg_type_mistmatch
                  end
                | [], [] -> return_type
              in
              helper arguments param_types
            | _ -> raise Not_arrow
    end

  (* | SeqAccess (var_name, inside_bracket) -> 
    
    
    begin
        match (lookup_in_context context var_name, check_expr context inside_bracket) with
        | (List ty, Int) -> ty
        | _ -> raise Bad_seq  
    end
  | StructCall (var_name, method_name, args) -> 
    begin
      let struct_type : ty = lookup_in_context context var_name in
      match struct_type with
      | StructShape (_, struct_context) ->
          let method_type = lookup_in_context struct_context method_name in
          begin
          match method_type with
          | Arrow (param_types, return_type) ->
              let rec helper arguments param_types = 
                match arguments, param_types with
                | arg :: args, ptype :: ptypes ->
                  let arg_type = check_expr context arg in
                  if arg_type = ptype 
                  then helper args ptypes
                  else raise Method_arg_type_mismatch
                | [], [] -> return_type
              in
              helper args param_types
          | _ -> raise Struct_not_arrow
          end
      | _ -> raise Not_a_struct
    end *)
  
(*   
| StructAccess (var_name, instance_var) -> 
    begin
      let struct_type = lookup_in_context context var_name in
      match struct_type with
      | Struct s -> 
        begin
          match lookup_in_context context s with
          | StructShape (_, struct_context) -> 
              lookup_in_context struct_context instance_var  
          | _ -> raise Not_found
        end
      | StructShape (_, struct_context) ->
        lookup_in_context struct_context instance_var
      | _ -> 
      print_endline "====accessing struct of type";
      print_endline (string_of_typ struct_type);
      print_context context;
print_endline "====finished accessing struct of type";
      raise Invalid_struct_access
    end
  | StructAssign (var_name, instance_var, expr) -> 
    begin

      let helper struct_context = 
        let expected_type = lookup_in_context struct_context instance_var in
        let actual_type = check_expr context expr in
        if expected_type = actual_type 
        then actual_type 
        else raise Instance_variable_type_mismatch
      in

      let struct_type = lookup_in_context context var_name in
      match struct_type with
      | StructShape (_, struct_context) ->
        helper struct_context
      | Struct s -> 
        match lookup_in_context context s with
        | StructShape (_, struct_context) -> helper struct_context
    end *)
  (* of string * string * expr *)
  | StructLit (struct_name, values) ->
    let struct_type = lookup_in_context context struct_name in
    match struct_type with
    | StructShape (_, struct_context) ->
    let rec helper values = 
      match values with
        | [] -> StructShape (struct_name, struct_context)
        | (name, e) :: vs ->    
          let expected = lookup_in_context struct_context name in
          let actual = check_expr context e in
          if expected = actual 
          then helper vs
          else raise Invalid_struct_lit
    in
    helper values


  
  
  
  (* of string * anon_decl list Name of struct and list of fields *)


;;

let check_func_def  (context : (ty * string) list) (f : func_def) : bool = 
  let parameters = f.parameters in
  let local_context = parameters @ context in
  let body = f.body in
  let rec check_stmts (context : (ty * string) list) (statements: stmt list)  : bool = 
      match statements with
        | [] -> true (* TODO: fix later *)
        | head :: tail ->
          match head with
          | Return e -> 
            if check_expr context e = f.return_type 
            then check_stmts context tail
            else false
          | If (expr, stmt) ->
            begin
            let expr_type = check_expr context expr in
            if expr_type <> Bool then false 
            else 
              if check_stmts context [stmt]
              then check_stmts context tail
              else false
            end
          | Block lst ->
            if check_stmts context lst 
            then check_stmts context tail
            else false
          | Expr expr ->
            ignore(check_expr context expr); 
            check_stmts context tail
          | Explicit ((ty, name),expr)->
            let expr_type : ty = check_expr context expr in
            if not (typ_eq expr_type ty) then false
            else check_stmts ((ty,name) :: context) tail
          | Define (name, expr) ->
            let expr_type = check_expr context expr in
            check_stmts ((expr_type,name) :: context) tail
          | IfElse (expr, stmt1, stmt2) ->
            begin
            let expr_type = check_expr context expr in
            if expr_type <> Bool then false 
            else 
            check_stmts context [stmt1] && 
            check_stmts context [stmt2] &&
            check_stmts context tail
            end

          | Iterate (x, e, stmt) ->
            let expr_type = check_expr context e in
            begin
              match expr_type with
              | List ty -> 
                  check_stmts ((ty, x) :: context) [stmt] && 
                  check_stmts context tail
              | _ -> false
            end
          | While (e, stmt) ->
            let expr_type = check_expr context e in
            if expr_type <> Bool then false 
            else check_stmts context [stmt] && 
                  check_stmts context tail
          (* in
          let checked = check_stmts context head in 
          if checked then
              checked && iterateStatements context tail
          else 
              begin
                print_endline "failed to typecheck!!!";
                print_endline (string_of_stmt head);
                false
              end *)
  in 
    check_stmts context body
;;

(* type struct_def = {
  struct_name: string;
  fields: bind list;
  methods: func_def list;
} *)

let bind_of_func_def f =
  let name = f.func_name in
  let typ = type_signature f in
  (typ, name)
  ;;
    


let check_struct_def (context: (ty * string) list) (sd: struct_def) : bool = 
  let struct_name = sd.struct_name in
  let fields = sd.fields in
  let methods = sd.methods in
  let method_signatures = List.map bind_of_func_def methods in
  let context = fields @ method_signatures @ context in
  let rec helper methods = match methods with
    | [] -> true (* Struct (fields @ method_signatures) *)
    | func_def :: rest -> 
      check_func_def context func_def;
      helper rest
  in
    helper methods

;;

let bind_of_struct_def sd = 
  let struct_name = sd.struct_name in
  let fields = sd.fields in
  let methods = sd.methods in
  let method_signatures = List.map bind_of_func_def methods in
  StructShape (struct_name, fields @ method_signatures), struct_name
;;

let check_program (context, func_defs, structs) =
  let func_def_binds = List.map bind_of_func_def func_defs in
  let struct_binds = List.map bind_of_struct_def structs in
  let context = func_def_binds @ struct_binds @ context in
 
print_endline "====start of printing context";
  print_context(context);
print_endline "===end of printing context";
    let rec funchelper func_defs = match func_defs with
    | [] -> ()
    | fd :: fds -> 
        if check_func_def context fd then funchelper fds
        else print_endline fd.func_name; raise Func_failed_typecheck
    in 
    funchelper func_defs
    let rec structhelper structs = match structs with
    | [] -> ()
    | st :: sts -> 
        if check_struct_def context st then structhelper sts
        else raise Struct_failed_typecheck
    in
    structhelper structs
;;