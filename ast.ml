(* The following defines the following operations +, -, *, / , && , ||, ==, !=, <, >, <=, >= *)
type op = Add | Sub | Mult | Div | And | Or | Equal | Neq | Less | Greater | Leq | Geq


type ty = AnyType | Int | Bool | Float | Char | String | Struct of string | List of ty

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
  | Declare of bind
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
  body: stmt;
}

(* A struct has a name, data fields, and functions *)

(* 
  struct Point {
    int x;
    int y;

    func getSum() int {
      return x + y;
    }
  }
*)
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
  | AnyType -> "AnyType"


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
  | Declare(b) -> string_of_bind b ^ ";\n"

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



