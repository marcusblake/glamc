exception TableEmpty

exception VariableAlreadyExists

exception UnrecognizedIdentifier of string

module StringMap = Map.Make (String)

type 'a symbol_table = {
  current_scope : 'a StringMap.t;
  parent : 'a symbol_table option;
}

let empty () = None

let add_scope table = Some { current_scope = StringMap.empty; parent = table }

(* Add the variable to the current scope. 
Takes in symbol_table, name of identifier, and type of identifier *)
let add_current_scope table name value =
  match table with
  | None -> raise TableEmpty
  | Some table ->
      let current_table = table.current_scope in
      if StringMap.mem name current_table then raise VariableAlreadyExists
      else
        let new_table = StringMap.add name value current_table in
        Some { current_scope = new_table; parent = table.parent }

(* Looks for identifier starting from current scope --> global scope. Takes in name of identifier and symbol_table *)
let rec lookup_identifier name = function
  | None ->
      raise
        (UnrecognizedIdentifier
           (Printf.sprintf "Unrecognized Identifier %s" name))
  | Some table -> (
      try StringMap.find name table.current_scope
      with Not_found -> lookup_identifier name table.parent )

(* Function to determine whether variable is within the scope of a function. Useful for computing closure *)
(* let rec in_function_scope level var = function
  | [] -> false
  | hd :: tl ->
    if level < 0 then false
    else if StringMap.mem var hd then true
    else in_function_scope (level-1) var tl *)
