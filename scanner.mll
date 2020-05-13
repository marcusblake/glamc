(* GlamC scanner *)

{ 
  open Parser

  (* This most likely needs to be changed. Easier way to detect chars????? *)
  let get_char input =
    if String.length input == 3 then input.[1]
    else if input.[2] == 'n' then '\n'
    else if input.[2] == 'r' then '\r'
    else '\t'

  let get_str input = List.nth (String.split_on_char '\"' input) 1
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let escaped_char = '\\' ('n' | 'r' | 't')
let exponent = ('e' | 'E') ('+' | '-')? digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| '.'      { DOT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { MULT }
| '/'      { DIV }
| '%'      { MODULUS }
| '='      { ASSIGN }
| ":="     { DEFINE }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| '>'      { GT }
| "<="     { LTEQ }
| ">="     { GTEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "for"    { FOR }
| "in"     { IN }
| "return" { RETURN }
| "int"    { INT }
| "string" { STRING }
| "float"  { FLOAT }
| "char"   { CHAR }
| "bool"   { BOOL }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "list"   { LIST }
| "struct" { STRUCT }
| "func"   { FUNC }
| digit+ as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| ''' (escaped_char | letter | digit) ''' as lem { CHARLIT(get_char lem) }
| '"' (letter | digit | escaped_char | ' ' | '_')* '"' as lem { STRLIT(get_str lem) }
| (digit+ '.' digit+ exponent?) | (digit+ exponent) | ('.' digit+ exponent?) as lem { FLOATLIT(float_of_string lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

(* {
   let to_string = function
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | LBRACKET -> "LBRACKET"
    | RBRACKET -> "RBRACKET"
    | SEMI -> "SEMI"
    | COLON -> "COLON"
    | COMMA -> "COMMA"
    | DOT -> "DOT"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | MULT -> "MULT"
    | DIV -> "DIV"
    | ASSIGN -> "ASSIGN"
    | DEFINE -> "DEFINE"
    | EQ -> "EQ"
    | NEQ -> "NEQ"
    | LT -> "LT"
    | GT -> "GT"
    | LTEQ -> "LTEQ"
    | GTEQ -> "GTEQ"
    | AND -> "AND"
    | OR -> "OR"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | WHILE -> "WHILE"
    | FOR -> "FOR"
    | IN -> "IN"
    | RETURN -> "RETURN"
    | INT -> "INT"
    | FLOAT -> "FLOAT"
    | CHAR -> "CHAR"
    | BOOL -> "BOOL"
    | BLIT(b)  -> Printf.sprintf "BLIT(%B)" b
    | LIST -> "LIST"
    | STRUCT -> "STRUCT"
    | FUNC -> "FUNC"
    | STRING -> "STRING"
    | LITERAL(d) -> Printf.sprintf "LITERAL(%d)" d
    | ID(id) -> Printf.sprintf "ID(%s)" id
    | CHARLIT(id) -> Printf.sprintf "CHARLIT(%s)" (Char.escaped id)
    | STRLIT(str) -> Printf.sprintf "STRLIT(%s)" str
    | FLOATLIT(f) -> Printf.sprintf "FLOATLIT(%f)" f
    | EOF -> "EOF" in
  let input = open_in "scanner_test.gc" in
  let lexbuf = Lexing.from_channel input in
  let rec loop prog = (function
    | EOF -> List.rev (to_string EOF::prog)
    | x -> loop (to_string x :: prog) (token lexbuf)) in
  let prog = String.concat " " (loop [] (token lexbuf)) in
  print_endline prog
} *)
