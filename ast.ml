(* The following defines the following operations +, -, *, / , && , ||, ==, !=, <, >, <=, >= *)
type op = Add | Sub | Mult | Div | And | Or | Equal | Neq | Less | Greater | Leq | Geq


(* TODO: How to define the type of a struct *)
type ty = Int | Bool | Float | Char | String | List of ty


type expr = 
  IntLit of int
  | BoolLit of int
  | FloatLit of float
  | CharLit of char
  | StringLit of string
  | Seq of expr list
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list


type stmt =
  Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt (* Figure out why two statements *)
  | While of expr * stmt
  | Return of expr

(* Declaration of a variable *)
type dec = ty * string

(* Define the type of a funcion *)
type func_def = {
  name: string;
  parameters: dec list;
  local_vars: dec list;
  return_type: ty;
  body: stmt list;
}

type program = dec list * func_def list




