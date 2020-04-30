(* The following defines the following operations +, -, *, / , && , ||, ==, !=, <, >, <=, >= *)
type op = Add | Sub | Mult | Div | And | Or | Equal | Neq | Less | Greater | Leq | Geq

type ty =
   | Int
   | Bool
   | Float
   | Char
   | String
   | Struct of (ty * string) list
   | List of ty 
   | Arrow of ty list * ty

type bind = ty * string

(* An expression is something that computes and returns a value *) 
type anon_decl = (string * expr)
and expr = 
  IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | CharLit of char
  | StringLit of string
  | StructLit of string * anon_decl list (* Name of struct and list of fields *)
  | Seq of expr list
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | SeqAccess of string * expr
  | StructCall of string * string * expr list (* name of identifier along with function call and list of arguments *)
  | StructAccess of string * string (* name of identifier and name of variable being accessed *)
  | StructAssign of string * string * expr

  
(* A statement is something that controls how the program is executed *)
type stmt =
  Block of stmt list
  | Expr of expr
  (* | Declare of bind *)
  | Explicit of bind * expr
  | Define of string * expr (* Will be used for := *)
  | If of expr * stmt
  | IfElse of expr * stmt * stmt
  | Iterate of string * expr * stmt
  | While of expr * stmt
  | Return of expr

(* Define the type of a funcion *)
type func_def = {
  func_name: string;
  parameters: bind list;
  return_type: ty;
  body: stmt list;
}

type func_def = {
  func_name: string;
  parameters: bind list;
  return_type: ty;
  body: stmt list;
}

let type_signature (func_def: func_def)  = 
  Arrow(List.map fst func_def.parameters, func_def.return_type)
  
(* A struct has a name, data fields, and functions *)
type struct_def = {
  struct_name: string;
  fields: bind list;
  methods: func_def list;
}

(* Our program consists of global variables, funtion definitions, and struct definitions *)
type program = bind list * func_def list * struct_def list

let fir = function
  | (a,_,_) -> a

let sec = function
  | (_,a,_) -> a

let third = function
  | (_,_,a) -> a

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Char -> "char"
  | String -> "string"
  | Struct d -> d
  | List d -> Printf.sprintf "list<%s>" (string_of_typ d)


let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | FloatLit(l) -> string_of_float l
  | CharLit(l) -> Char.escaped l
  | StringLit(l) -> l
  | StructLit(t, d) -> t ^ "{" ^ String.concat ", " (List.map (fun (s, e) -> s ^ ": " ^ string_of_expr e) d) ^ "}"
  | Seq(s) -> "[" ^ String.concat ", " (List.map string_of_expr s) ^ "]"
  | SeqAccess(s,e) -> s ^ "[" ^ string_of_expr e ^ "]"
  | StructCall(st, fn, ps) -> st ^ "." ^ fn ^ "(" ^ String.concat ", " (List.map string_of_expr ps) ^ ")"
  | StructAccess(st, fld) -> st ^ "." ^ fld
  | StructAssign(st, fld, e) -> st ^ "." ^ fld ^ " = " ^(string_of_expr e)

let string_of_bind (t, id) = string_of_typ t ^ " " ^ id

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1
  | IfElse(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Define(v, e) -> v ^ " := " ^ string_of_expr e ^ ";\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Explicit(b, e) -> string_of_bind b ^ " = " ^ string_of_expr e ^ ";\n"
  | Iterate(v, e, s) -> "for (" ^ v ^ " in " ^ string_of_expr e ^ ") " ^ string_of_stmt s

(* let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n" *)


let string_of_fdecl func =
  "func " ^ func.func_name ^ "(" ^ 
  String.concat ", " (List.map string_of_bind func.parameters) ^ ") " ^
  string_of_typ func.return_type ^ " {\n" ^
  String.concat "" (List.map string_of_stmt func.body) ^
  "\n}\n"

let string_of_struct struct_type = 
  "struct " ^ struct_type.struct_name ^ "{\n" ^
  String.concat ";\n" (List.map string_of_bind struct_type.fields) ^ ";\n" ^
  String.concat "\n" (List.map string_of_fdecl struct_type.methods) ^
  "\n}\n"

let string_of_program (vars, funcs, structs) =
  "\n\nParsed program: \n\n" ^
  String.concat ";\n" (List.map string_of_bind vars) ^ ";\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_struct structs)
  
let rec lookup_in_context context str = 
  match context with
  | (t, name) :: rest-> 
      if name = str then t else lookup_in_context rest str
  | [] -> raise Not_found
  
let check_expr (context : (ty * string) list) (expr: expr) : ty = 
  match expr with
  | IntLit _ -> Int
  | BoolLit _ -> Bool
  | FloatLit _ -> Float
  | CharLit _ -> Char
  | StringLit _ -> string
  | Seq lst -> 
    begin
      match List.rev lst with
      | last_expr :: xs -> check_expr context last_expr
      | _ -> raise Not_found
    end
  | Id name -> lookup_in_context context name
  | Binop (lhs, op, rhs) ->
    begin
      match op with
        | Add 
        | Sub
        | Mult
        | Div -> 
          begin
             match (check_expr lhs, check_expr rhs) with
             | (Int, Int) -> Int
             | _ -> raise Not_found
          end
        | And
        | Or -> 
          begin
             match (check_expr lhs, check_expr rhs) with
             | (Bool, Bool) -> Bool
             | _ -> raise Not_found
          end
        | Equal ->
          begin
             match (check_expr lhs, check_expr rhs) with
             | (x, y) -> if x = y then x else raise Not_found
             | _ -> raise Not_found
          end
        | Neq | Less | Greater | Leq | Geq -> 
          begin
             match (check_expr lhs, check_expr rhs) with
             | (Float, Float) -> Float
             | _ -> raise Not_found
          end
    end
  
  | Assign (name, e) -> 
    check_expr context e
  | Call (name, arguments) -> 
    begin
      let funcType = lookup_in_context context name
      match funcType with
          | Arrow (param_types, return_type) ->
              let rec helper arguments param_types = 
                match arguments, param_types with
                | arg :: args, ptype :: ptypes ->
                  let arg_type = check_expr context arg in
                  if arg_type = ptype 
                  then helper args ptypes
                  else raise Not_found
                | [], [] -> return_type
              in
              helper arguments param_types
          | _ -> raise Not_found
    end
   of string * expr list
  | SeqAccess (var_name, inside_bracket) -> 
    begin
        match (lookup_in_context context var_name, check_expr inside_bracket) with
        | (List ty, Int) -> ty
        | _ -> raise Not_found  
    end
  | StructCall (var_name, method_name, args) -> 
    begin
      let struct_type = lookup_in_context context var_name in
      match struct_type with
      | StructType struct_context ->
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
                  else raise Not_found
                | [], [] -> return_type
              in
              helper arguments param_types
          | _ -> raise Not_found
          end
      | _ -> raise Not_found
    end
  
  
| StructAccess (var_name, instance_var) -> 
    begin
      let struct_type = lookup_in_context context var_name in
      match struct_type with
      | StructType struct_context ->
        lookup_in_context struct_context instance_var
    end
  | StructAssign (var_name, instance_var, expr) -> 
    begin
      let struct_type = lookup_in_context context var_name in
      match struct_type with
      | StructType struct_context ->
        let expected_type = lookup_in_context struct_context instance_var in
        let actual_type = check_expr context expr in
        if expected_type = actual_type 
        then actual_type 
        else raise Not_found
    end
  (* of string * string * expr *)
  | StructLit (struct_name, values) ->
    let struct_type = lookup_in_context context struct_name in
    match struct_type with
    | Struct (struct_context) ->
    let rec helper values = 
      match values with
        | [] ->   Struct (struct_context)
        | (name, e) :: vs ->    
          let expected = lookup_in_context struct_context name in
          let actual = check_expr context e in
          if expected = actual then helper vs
          else raise Not_found
    in
    helper values
  
let check_func  (context : (ty * string) list) (f : func_def) : context = 
  let parameters : (ty * string) list = f.parameters in
  let local_context = parameters @ context in
  let body : stmt list = f.body in
  let rec iterateStatements (context : (ty * string) list) (statements: stmt list)  : bool = 
    match statements with
    | [] -> context
    | head :: tail
    match head with
      | Return e -> 
         check_expr e = f.return_type
      | If (expr, stmt) ->
        begin
         let expr_type = check_expr context expr in
         if expr_type <> Bool then false 
         else check_stmt context stmt
        end
      | Block lst ->
        iterateStatements context lst 
      | Expr expr ->
        check_expr context expr
      | Explicit ((ty, name),expr)->
        let expr_type = check_expr expr in
        if expr_type <> ty then false
        else iterateStatements ((ty,name) :: context) tail
      | Define (name, expr) ->
        let expr_type = check_expr expr in
        iterateStatements ((ty,name) :: context) tail
      | IfElse (expr, stmt1, stmt2) ->
        begin
         let expr_type = check_expr context expr in
         if expr_type <> Bool then false 
         else check_stmt context stmt1 && check_stmt context stmt2
        end

      | Iterate (x, e, stmt) ->
        let expr_type = check_expr context e in
        begin
          match expr_type with
          | List _ -> check_stmt stmt
          | _ -> false
        end
      | While (e, stmt) ->
         let expr_type = check_expr context expr in
         if expr_type <> Bool then false 
         else check_stmt context stmt1 && check_stmt context stmt2
  in 
    iterateStatements body
;;

let bind_of_func_def f =
  let name = f.func_name in
  let typ = type_signature f in
  (typ, name)
  ;;

let check_struct_def (context: (ty * string) list) (sd: struct_def) : context = 
  let struct_name = sd.struct_name in
  let fields = sd.fields in
  let methods = sd.methods in
  let method_signatures = List.map bind_of_func_def methods in
  let context = fields @ method_signatures @ context in
  let rec helper methods = match methods with
    | [] -> Struct (fields @ method_signatures)
    | func_def :: rest -> 
      check_func_def context func_def;
      helper rest
  in
    helper methods

;;

let bind_of_struct_def = 
  let struct_name = sd.struct_name in
  let fields = sd.fields in
  let methods = sd.methods in
  let method_signatures = List.map bind_of_func_def methods in
  fields @ method_signatures
;;

let check_program (context, func_defs, structs) =
  let func_def_binds = List.map bind_of_func_def methods in
  let struct_binds = List.map bind_of_struct_def structs in
  let context = func_def_binds @ context in
  begin
    let rec funchelper = match func_defs with
    | [] -> ()
    | fd :: fds -> 
        check_func_def context fd; 
        funchelper fds
    in 
    funchelper func_defs
  end;
  begin
    let rec structhelper = match structs with
    | [] -> ()
    | st :: sts -> 
        check_func_def context st; 
        structhelper sts
    in
    structhelper structs
  end
;;



