(* The following defines the following operations +, -, *, / , && , ||, ==, !=, <, >, <=, >= *)
type op = Add | Sub | Mult | Div | And | Or | Equal | Neq | Less | Greater | Leq | Geq | Mod

type uop = Neg | Not

type ty = AnyType | Type of ty | Void | Int | Bool | Float | Char | String | Struct of string | List of ty | Function of (ty list * ty)

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
  | FunctionLit of func_def
  | TypeLit of ty
  | Seq of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list
  | SeqAccess of expr * expr
  | StructCall of string * string * expr list (* name of identifier along with function call and list of arguments *)
  | StructAccess of string * string (* name of identifier and name of variable being accessed *)
and stmt =
  Block of stmt list
  | Expr of expr
  | Declare of bind
  | Explicit of bind * expr
  | Define of string * expr (* Will be used for := *)
  | Assign of string * expr
  | AssignSeq of expr * expr * expr
  | StructAssign of string * string * expr
  | If of expr * stmt
  | IfElse of expr * stmt * stmt
  | Iterate of string * expr * stmt
  | Range of string * expr * expr * stmt
  | While of expr * stmt
  | Return of expr
and func_def = {
  func_name: string;
  parameters: bind list;
  return_type: ty;
  body: stmt;
  heap_vars: string list;
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