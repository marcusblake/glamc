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
  ]



(* BEGIN: Helper function for symbol table *)




(* Adds a new (current) scope to symbol_table list to the beginning of the list *)
let add_scope table = StringMap.empty :: table



let check_assign lvaluet rvaluet err = if lvaluet = rvaluet then lvaluet else raise (Failure err)



(* Add the variable to the current scope. 
Takes in symbol_table, name of identifier, and type of identifier *)
let add_to_current_scope table name value =
  if StringMap.mem name (List.hd table) then raise VariableAlreadyExists
  else List.mapi (fun idx map -> if idx = 0 then StringMap.add name value map else map) table



(* Looks for identifier starting from current scope --> global scope. Takes in name of identifier and symbol_table *)
let rec lookup_identifier name = function
  | [] -> raise (UnrecognizedIdentifier (Printf.sprintf "Unrecognized Identifier %s" name))
  | current_scope :: tl -> 
    try
      StringMap.find name current_scope
    with Not_found ->
      lookup_identifier name tl


(* Function to determine whether variable is within the scope of a function. Useful for computing closure *)
let rec in_function_scope level var = function
  | [] -> false
  | hd :: tl ->
    if level < 0 then false
    else if StringMap.mem var hd then true
    else in_function_scope (level-1) var tl



(* END: Helper function for symbol table *)




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


  