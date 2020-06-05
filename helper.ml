open Ast
open Exceptions
open Printing


(* Map used for symbol table *)
module StringMap = Map.Make(String)

let built_in_functions = [
    {
      return_type = Int;
      func_name = "println";
      parameters = [(AnyType, "x")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = Int;
      func_name = "len";
      parameters = [(AnyType, "x")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = Int;
      func_name = "append";
      parameters = [(List AnyType, "x"); (AnyType, "y")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = Int;
      func_name = "put";
      parameters = [(List AnyType, "x"); (Int, "y") ;(AnyType, "z")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = Int;
      func_name = "pop";
      parameters = [(List AnyType, "x")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = Int;
      func_name = "map";
      parameters = [(List AnyType, "x"); (Function(([AnyType], AnyType)), "y")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = Int;
      func_name = "float_to_int";
      parameters = [(Float, "x")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = Float;
      func_name = "int_to_float";
      parameters = [(Int, "x")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = Int;
      func_name = "char_to_int";
      parameters = [(Char, "x")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = Char;
      func_name = "int_to_char";
      parameters = [(Int, "x")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = String;
      func_name = "read";
      parameters = [(String, "x")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = Int;
      func_name = "write";
      parameters = [(String, "filename"); (String, "str")];
      body = Block [];
      heap_vars = []
    };
    {
      return_type = List String;
      func_name = "split";
      parameters = [(String, "str"); (Char, "del")];
      body = Block [];
      heap_vars = []      
    };
    {
      return_type = String;
      func_name = "join";
      parameters = [(List String, "str"); (Char, "del")];
      body = Block [];
      heap_vars = []      
    }
  ]


let check_assign lvaluet rvaluet err = if lvaluet = rvaluet then lvaluet else raise (Failure err)


(* BEGIN: Helper functions for functions *)

let func_exists map function_name = StringMap.mem function_name map

(* Adds a function to a map that maps function name to the function definition struct *)
let add_func map func =
  if func_exists map func.func_name then raise FunctionAlreadyExists
  else StringMap.add func.func_name func map


(* Tries to find function inside of map. Raises error if function doesn't exist *)
let find_func map function_name = 
  try
    StringMap.find function_name map
  with Not_found -> raise (FunctionDoesntExist("Couldn't find function " ^ function_name))


(* END: Helper functions for functions *)



let is_iterable type_ = 
  begin match type_ with
  String | List _ -> true
  | _ -> false
  end

let get_element_type type_ =
  begin match type_ with 
  List ty -> ty
  | String -> Char
  | _ -> raise Invalid
  end

let struct_name type_ =
  begin match type_ with
  Struct name -> name
  | _ -> raise Invalid
  end


  