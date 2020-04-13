%{
open Ast
%}

(* Need to coordinate naming for tokens *)
%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN
%token EQ NEQ LT AND OR
%token IF ELSE WHILE INT BOOL
%token RETURN COMMA
%token <int> LITERAL
%token <bool> BLIT
/* %token <char> CHAR */ (* Need to recognize chars in lexer *)
/* %token <float> FLOAT */ (* Need to recognize floats in lexer *)
%token <string> ID
/* %token <string> STRING */ (* Lexer needs to be able to detect strings. Need a regex for strings *)
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left PLUS MINUS



%%



(* Need to define func keyword in scanner *)
func_decl:
  FUNC ID LPAREN params_list RPAREN type_ LBRACE stmt_list RBRACE
  {
    {
      name: $2;
      parameters: $4;
      return_type: $6;
      body: $8;
    }
  }

params_list:
  /*nothing*/ { [] }
  | vardecl COMMA params_list { $1::$3 }

(* CFG for defining a variable (for example int x;) *)
vardecl:
  type_ ID { ($1, $2) }

type_:
    INT { Int }
  | BOOL { Bool }
  | FLOAT { Float }
  | CHAR { Char }
  (* Need string definition in scanner *)
  | STRING { String }
  | LIST LT type_ GT { List $3 } (* Need to define greater than in scanner *)


expr_list:
  /* nothing */ { [] }
  | expr COMMA expr_list { $1::$2 }

expr:
    ID { Id($1) }
  | LITERAL { IntLit($1) }
  | BLIT { Boollit($1) }
  (* Need square brackets defined for detecting lists *)
  | LBRACKET expr_list RBRACKET { Seq($1) }
  | expr PLUS expr { Binop($1, Add, $3) }
  | expr MINUS expr { Binop($1, Sub, $3) }
  | expr EQ expr { Binop($1, Equal, $3) }
  | expr NEQ expr { Binop($1, Neq, $3) }
  | expr LT expr { Binop($1, Less, $3) }
  /* | expr GT expr { Binop($1, Greater, $3) } Needs to be implemented in scanner*/
  | expr AND expr { Binop($1, And, $3) }
  | expr OR expr { Binop($1, Or, $3) }
  | ID LPAREN expr_list RPAREN { Call($1, $3) }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list { $1::$2 }

stmt:
    expr SEMI                 { Expr $1 }
    (* For variable declaration such as int a; *)
  | LBRACE stmt_list RBRACE   { Block $2 }
  | vardecl SEMI              { Bind($1) }
  | vardecl ASSIGN expr SEMI  { Explicit($1, $2) }
  | ID ASSIGN expr SEMI       { Assign($1, $3) }
  | ID DEFINE  expr SEMI      { Define($1, $3) }
  | RETURN expr SEMI          { Return $2 }
  /* | FOR LPAREN ID IN ID RPAREN stmt { Iterate($2, $3) } Most likely needs to change */
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  /* | IF LPAREN expr RPAREN stmt ELSE stmt  TODO: think about this more*/

