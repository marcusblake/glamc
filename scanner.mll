(* GlamC scanner *)

{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

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
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

(* {
   let to_string = function
    | LPAREN -> Printf.sprintf "LPAREN"
    | RPAREN -> Printf.sprintf "RPAREN"
    | LBRACE -> Printf.sprintf "LBRACE"
    | RBRACE -> Printf.sprintf "RBRACE"
    | LBRACKET -> Printf.sprintf "LBRACKET"
    | RBRACKET -> Printf.sprintf "RBRACKET"
    | SEMI -> Printf.sprintf "SEMI"
    | COLON -> Printf.sprintf "COLON"
    | COMMA -> Printf.sprintf "COMMA"
    | DOT -> Printf.sprintf "DOT"
    | PLUS -> Printf.sprintf "PLUS"
    | MINUS -> Printf.sprintf "MINUS"
    | MULT -> Printf.sprintf "MULT"
    | DIV -> Printf.sprintf "DIV"
    | ASSIGN -> Printf.sprintf "ASSIGN"
    | DEFINE -> Printf.sprintf "DEFINE"
    | EQ -> Printf.sprintf "EQ"
    | NEQ -> Printf.sprintf "NEQ"
    | LT -> Printf.sprintf "LT"
    | GT -> Printf.sprintf "GT"
    | LTEQ -> Printf.sprintf "LTEQ"
    | GTEQ -> Printf.sprintf "GTEQ"
    | AND -> Printf.sprintf "AND"
    | OR -> Printf.sprintf "OR"
    | IF -> Printf.sprintf "IF"
    | ELSE -> Printf.sprintf "ELSE"
    | WHILE -> Printf.sprintf "WHILE"
    | FOR -> Printf.sprintf "FOR"
    | IN -> Printf.sprintf "IN"
    | RETURN -> Printf.sprintf "RETURN"
    | INT -> Printf.sprintf "INT"
    | FLOAT -> Printf.sprintf "FLOAT"
    | CHAR -> Printf.sprintf "CHAR"
    | BOOL -> Printf.sprintf "BOOL"
    | BLIT(b)  -> Printf.sprintf "BLIT(%B)" b
    | LIST -> Printf.sprintf "LIST"
    | STRUCT -> Printf.sprintf "STRUCT"
    | FUNC -> Printf.sprintf "FUNC"
    | LITERAL(d) -> Printf.sprintf "LITERAL(%d)" d
    | ID(id) -> Printf.sprintf "ID(%s)" id
    | EOF -> Printf.sprintf "EOF" 
    | _ -> Printf.sprintf "N/A" in
  let input = open_in "scanner_test.gc" in
  let lexbuf = Lexing.from_channel input in
  let rec loop prog = (function
    | EOF -> List.rev (to_string EOF::prog)
    | x -> loop (to_string x :: prog) (token lexbuf)) in
  let prog = String.concat " " (loop [] (token lexbuf)) in
  print_endline prog
} *)
