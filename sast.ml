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
  | SUop of uop * sexpr
  | SCall of string * sexpr list
  | SSeqAccess of sexpr * sexpr
  | SStructCall of string * string * sexpr list (* name of identifier along with function call and list of arguments *)
  | SStructAccess of string * string (* name of identifier and name of variable being accessed *)

  
(* A statement is something that controls how the program is executed *)
type sstmt =
  SBlock of sstmt list
  | SExpr of sexpr
  | SDeclare of bind
  | SExplicit of bind * sexpr
  | SDefine of string * sexpr (* Will be used for := *)
  | SIf of sexpr * sstmt
  | SIfElse of sexpr * sstmt * sstmt
  | SIterate of string * sexpr * sstmt
  | SRange of string * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SReturn of sexpr
  | SAssign of string * sexpr
  | SStructAssign of string * string * sexpr

(* Define the type of a funcion *)
type sfunc_def = {
  sfunc_name: string;
  sparameters: bind list;
  sreturn_type: ty;
  sbody: sstmt;
}

(* A struct has a name, data fields, and functions *)
type sstruct_def = {
  sstruct_name: string;
  sfields: bind list;
  smethods: sfunc_def list;
}

(* Our program consists of global variables, funtion definitions, and struct definitions *)
type sprogram = bind list * sfunc_def list * sstruct_def list

