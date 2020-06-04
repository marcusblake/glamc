(* GlamC scanner *)

{ 
  open Parser

  exception InvalidEscape

  let match_char = function
  'n' -> '\n'
  | 'r' -> '\r'
  | 't' -> '\t'
  | _ -> raise InvalidEscape


  (* This most likely needs to be changed. Easier way to detect chars????? *)
  let get_char input =
    if String.length input = 3 then input.[1]
    else match_char input.[2]

  let get_str input = 
    let n = String.length input in
    let esc_char = function
      'n' | 'r' | 't' -> true
      | _ -> false
    in
    let rec helper idx str = 
      if idx = 0 then helper (idx+1) str
      else if idx = (n-1) then  ""
      else (
        let c = str.[idx] in
        if c = '\\' && esc_char str.[idx+1] then String.make 1 (match_char str.[idx+1]) ^ helper (idx+2) str
        else String.make 1 c ^ helper(idx+1) str
      )
    in
    helper 0 input
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let escaped_char = '\\' ('n' | 'r' | 't')
let exponent = ('e' | 'E') ('+' | '-')? digit+

rule token = parse
  [' ' '\t' '\r'] { token lexbuf } (* Whitespace *)
| '\n'     { Lexing.new_line lexbuf; token lexbuf }
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
| "..."    { ELIPS }
| "+="     { PLEQ }
| "-="     { SUBEQ }
| "*="     { MLTEQ }
| "/="     { DIVEQ }
| "%="     { MODEQ }
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
| "var"    { VAR }
| "struct" { STRUCT }
| "func"   { FUNC }
| "->"     { ARROW }
| digit+ as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| ''' (escaped_char | letter | digit| ' ' | '_' | '%' | '.') ''' as lem { CHARLIT(get_char lem) }
| '"' (letter | digit | escaped_char | ' ' | '_' | '%' | '.')* '"' as lem { STRLIT(get_str lem) }
| (digit+ '.' digit+ exponent?) | (digit+ exponent) | ('.' digit+ exponent?) as lem { FLOATLIT(float_of_string lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

{
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
  | PLEQ -> "PLEQ"
  | SUBEQ -> "SUBEQ"
  | MLTEQ -> "MLTEQ"
  | DIVEQ -> "DIVEQ"
  | MODEQ -> "MODEQ"
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
  | MODULUS -> "MODULUS"
  | ELIPS -> "ELIPS"
  | NOT -> "NOT"
  | VAR -> "VAR"
  | BLIT(b)  -> Printf.sprintf "BLIT(%B)" b
  | LIST -> "LIST"
  | STRUCT -> "STRUCT"
  | FUNC -> "FUNC"
  | STRING -> "STRING"
  | ARROW -> "ARROW"
  | LITERAL(d) -> Printf.sprintf "LITERAL(%d)" d
  | ID(id) -> Printf.sprintf "ID(%s)" id
  | CHARLIT(id) -> Printf.sprintf "CHARLIT(%s)" (Char.escaped id)
  | STRLIT(str) -> Printf.sprintf "STRLIT(%s)" str
  | FLOATLIT(f) -> Printf.sprintf "FLOATLIT(%f)" f
  | EOF -> "EOF"

  let string_of_tokens file =
    let input = open_in file in
    let lexbuf = Lexing.from_channel input in
    let rec loop prog = (function
      | EOF -> List.rev (to_string EOF::prog)
      | x -> loop (to_string x :: prog) (token lexbuf)) in
    String.concat " " (loop [] (token lexbuf))
}

