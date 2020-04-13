(* The following defines the following operations +, -, *, / , && , ||, ==, !=, <, >, <=, >= *)
type op = Add | Sub | Mult | Div | And | Or | Equal | Neq | Less | Greater | Leq | Geq


(* TODO: How to define the type of a struct *)
type ty = Int | Bool | Float | Char | String | Struct | List of ty


(* An expression is something that computes and returns a value *) 
type anon_decl = (string * expr) and
type expr = 
  IntLit of int
  | BoolLit of int
  | FloatLit of float
  | CharLit of char
  | StringLit of string
  | Seq of expr list
  (* | StructLit of (string * anon_decl list) Name of struct and list of fields *)
  | Id of string
  | Binop of expr * op * expr
  | Call of string * expr list

type bind = ty * string

(*
  struct name {
    int x;
    int y;
  }
*)
(* type struct_def = {
  name: string;
  fields: stmt list
} *)

(* A statement is something that controls how the program is executed *)
type stmt =
  Block of stmt list
  | Expr of expr
  | Assign of string * expr
  | Declare of bind
  | Explicit of bind * expr
  | Define of string * expr (* Will be used for := *)
  | If of expr * stmt * stmt (* Figure out why two statements *)
  | Iterate of string * expr
  | While of expr * stmt
  | Return of expr

(* Define the type of a funcion *)
type func_def = {
  name: string;
  parameters: bind list;
  return_type: ty;
  body: stmt list;
}

type program = dec list * func_def list
