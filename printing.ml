open Ast
open Parser

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Char -> "char"
  | String -> "string"
  | Struct d -> d
  | List d -> Printf.sprintf "list<%s>" (string_of_typ d)
  | Function(args, rty) -> "func (" ^ String.concat ", " (List.map string_of_typ args) ^ ") " ^ string_of_typ rty
  | AnyType -> "AnyType"

let string_of_bind (t, id) = "var " ^  id ^ " " ^ string_of_typ t


let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | FloatLit(l) -> string_of_float l
  | CharLit(l) -> Char.escaped l
  | StringLit(l) -> l
  | FunctionLit(func) -> "func (" ^ 
    String.concat ", " (List.map string_of_bind func.parameters) ^ 
    ") " ^ string_of_typ func.return_type ^ " -> " ^
    string_of_stmt func.body
  | StructLit(t, d) -> t ^ "{" ^ String.concat ", " (List.map (fun (s, e) -> s ^ ": " ^ string_of_expr e) d) ^ "}"
  | Seq(s) -> "[" ^ String.concat ", " (List.map string_of_expr s) ^ "]"
  | SeqAccess(s,e) -> string_of_expr s ^ "[" ^ string_of_expr e ^ "]"
  | StructCall(st, fn, ps) -> st ^ "." ^ fn ^ "(" ^ String.concat ", " (List.map string_of_expr ps) ^ ")"
  | StructAccess(st, fld) -> st ^ "." ^ fld
and string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1
  | IfElse(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Define(v, e) -> v ^ " := " ^ string_of_expr e ^ ";\n"
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e ^ ";\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Explicit(b, e) -> string_of_bind b ^ " = " ^ string_of_expr e ^ ";\n"
  | Iterate(v, e, s) -> "for (" ^ v ^ " in " ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Range(v, e, e1, s) -> "for (" ^ v ^ " in " ^ string_of_expr e ^ "..." ^ string_of_expr e1 ^ ") " ^ string_of_stmt s
  | Declare(b) -> string_of_bind b ^ ";\n"
  | StructAssign(st, fld, e) -> st ^ "." ^ fld ^ " = " ^(string_of_expr e) ^ ";\n"

(* let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n" *)


let string_of_fdecl func =
  "func " ^ func.func_name ^ "(" ^ 
  String.concat ", " (List.map string_of_bind func.parameters) ^ ") " ^
  string_of_typ func.return_type ^
  string_of_stmt func.body

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