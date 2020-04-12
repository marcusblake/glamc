%{
open Ast
%}

(* Need to coordinate naming for tokens *)
%token <int> LITERAL
%token <bool> BLIT
/* %token <char> CHAR */ (* Need to recognize chars in lexer *)
/* %token <float> FLOAT */ (* Need to recognize floats in lexer *)
%token <string> ID
/* %token <string> STRING */ (* Lexer needs to be able to detect strings. Need a regex for strings *)


%%



(* Need to define func keyword in scanner *)
func_decl:
  FUNC ID LPAREN params_list RPAREN type_ LBRACE local_vars stmt_list RBRACE
  {
    {
      name: $2;
      parameters: $4;
      local_vars: $8;
      return_type: $6;
      body: $9;
    }
  }


(* CFG for defining a variable (for example int x;) *)
vardecl:
  type_ ID { ($1, $2) }


type_:
    INT { Int }
  | BOOL { Bool }
  | FLOAT { Float }
  | CHAR { Char }
  (* Need string definition in scanner *)
  | LIST LT type_ GT { List $3 } (* Need to define greater than in scanner *)

expr:
    ID { Id($1) }
  | BLIT { Boollit($1) }
  (* Need square brackets defined for detecting lists *)
  