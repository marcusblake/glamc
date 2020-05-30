type 'a symbol_table

val empty : unit -> 'a symbol_table option

val add_scope : 'a symbol_table option -> 'a symbol_table option

val add_current_scope : 'a symbol_table option -> string -> 'a -> 'a symbol_table option

val lookup_identifier : string -> 'a symbol_table option -> 'a

