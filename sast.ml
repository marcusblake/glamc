open Ast

(* An expression is something that computes and returns a value *)
type sexpr = (ty * sex)
and sanon_decl = (string * sexpr)
and sex = 
  SIntLit of int
  | SBoolLit of bool
  | SFloatLit of float
  | SCharLit of char
  | SStringLit of string
  | SStructLit of string * sanon_decl list (* Name of struct and list of fields *)
  | SSeq of sexpr list
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SSeqAccess of string * sexpr
  | SStructCall of string * string * sexpr list (* name of identifier along with function call and list of arguments *)
  | SStructAccess of string * string (* name of identifier and name of variable being accessed *)
  | SStructAssign of string * string * sexpr

  
(* A statement is something that controls how the program is executed *)
type sstmt =
  SBlock of sstmt list
  | SExpr of sexpr
  (* | Declare of bind *)
  | SExplicit of bind * sexpr
  | SDefine of string * sexpr (* Will be used for := *)
  | SIf of sexpr * sstmt
  | SIfElse of sexpr * sstmt * sstmt
  | SIterate of string * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SReturn of sexpr

(* Define the type of a funcion *)
type sfunc_def = {
  func_name: string;
  parameters: bind list;
  return_type: ty;
  body: sstmt list;
}

(* A struct has a name, data fields, and functions *)
type sstruct_def = {
  struct_name: string;
  fields: bind list;
  methods: sfunc_def list;
}

(* Our program consists of global variables, funtion definitions, and struct definitions *)
type program = bind list * sfunc_def list * sstruct_def list

