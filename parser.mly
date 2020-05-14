%{
open Ast
%}

/* Need to coordinate naming for tokens */
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET PLUS MINUS MULT DIV ASSIGN DEFINE
%token EQ NEQ LT GT LTEQ GTEQ AND OR
%token IF ELSE WHILE FOR IN INT BOOL FLOAT CHAR STRING STRUCT LIST
%token RETURN COMMA COLON DOT FUNC
%token <int> LITERAL
%token <bool> BLIT
%token <char> CHARLIT /* Need to recognize chars in lexer */
%token <float> FLOATLIT /* Need to recognize floats in lexer */
%token <string> ID
%token <string> STRLIT /* Lexer needs to be able to detect strings. Need a regex for strings */
%token EOF


%start program
%type <Ast.program> program


%right DEFINE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LTEQ GT GTEQ
%left PLUS MINUS
%left MULT DIV



%%

program:
  decls EOF { $1 }

decls:
  /* nothing */ { ([], [], []) }
  | vardecl SEMI decls { (($1 :: fir $3), sec $3, third $3) }
  | func_decl decls { (fir $2, ($1 :: sec $2), third $2)  }
  | struct_decl decls { (fir $2, sec $2, ($1 :: third $2)) }


/* Need to define func keyword in scanner */
func_decl:
  FUNC ID LPAREN vardecl_parms RPAREN type_ block
  {
    {
      func_name = $2;
      parameters = $4;
      return_type = $6;
      body = $7
    }
  }

struct_decl:
  STRUCT ID LBRACE vardecls_semi func_decls RBRACE
  {
    {
      struct_name = $2;
      fields = $4;
      methods = $5
    }
  }

func_decls:
  /* NOTHING */ { [] }
  | func_decl func_decls { $1 :: $2 }

vardecls_semi:
  /* NOTHING */ { [] }
  | vardecl SEMI vardecls_semi { $1 :: $3 }


vardecl_parms:
    /* NOTHING */ { [] }
  | vardecls_comma { $1 }

vardecls_comma:
  | vardecl { [$1] }
  | vardecl COMMA vardecls_comma { $1 :: $3 }


/* CFG for defining a variable (for example int x;) */
vardecl:
  type_ ID { ($1, $2) }

type_:
    INT               { Int }
  | BOOL              { Bool }
  | FLOAT             { Float }
  | CHAR              { Char }
  | STRING            { String } /* Need string definition in scanner */
  | ID                { Struct $1 }
  | LIST LT type_ GT  { List $3 } /* Need to define greater than in scanner */


expr_params:
    /* NOTHING */ { [] }
  | expr_list     { $1 }

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }

anon_decl:
  ID COLON expr { ($1, $3) }

struct_fields:
  /* NOTHING */ { [] }
  | field_list { $1 }

field_list:
  anon_decl { [$1] }
  | anon_decl COMMA field_list { $1 :: $3 }

expr:
    ID                                   { Id($1) } /* varName */
  | LITERAL                              { IntLit($1) } /* 2 */
  | BLIT                                 { BoolLit($1) } /* true */
  | FLOATLIT                             { FloatLit($1) } /* 3.22 */
  | CHARLIT                              { CharLit($1) } /* 'c' */
  | STRLIT                               { StringLit($1) } /*  "helloworld" */
  | ID LBRACE struct_fields RBRACE       { StructLit($1, $3) } /* Point{x: 2, y: 3}  */
  | LBRACKET expr_params RBRACKET        { Seq($2) } /* [2,3,4,5] */
  | expr PLUS expr                       { Binop($1, Add, $3) } /* 2 + 3 */
  | expr MINUS expr                      { Binop($1, Sub, $3) } /* 5 - 2 */
  | expr MULT expr                       { Binop($1, Mult, $3) } /* 6 * 3 */
  | expr DIV expr                        { Binop($1, Div, $3) } /* 9 / 3 */
  | expr EQ expr                         { Binop($1, Equal, $3) } /* 2 == 4 */
  | expr NEQ expr                        { Binop($1, Neq, $3) } /* 9 != 9 */
  | expr LT expr                         { Binop($1, Less, $3) } /* 2 < 3 */
  | expr GT expr                         { Binop($1, Greater, $3) } /* 6 > 0 */
  | expr GTEQ expr                       { Binop($1, Geq, $3) } /* 2 >= 3 */
  | expr LTEQ expr                       { Binop($1, Leq, $3) } /* 2 <= 3 */
  | expr AND expr                        { Binop($1, And, $3) } /* true && false */
  | expr OR expr                         { Binop($1, Or, $3) } /* true || false */
  | ID ASSIGN expr                       { Assign($1, $3) } /* x = 2 */
  | ID LBRACKET expr RBRACKET            { SeqAccess($1, $3) }
  | ID LPAREN expr_params RPAREN         { Call($1, $3) }
  | ID DOT ID LPAREN expr_params RPAREN  { StructCall($1, $3, $5) } /* point.toString() */
  | ID DOT ID                            { StructAccess($1, $3) }
  | ID DOT ID ASSIGN expr                { StructAssign($1, $3, $5) }


stmt_list:
  /* nothing */     { [] }
  | stmt stmt_list  { $1 :: $2 }

block:
  LBRACE stmt_list RBRACE { Block $2 }


if_stmt:
  | IF LPAREN expr RPAREN block             { If($3,$5) }

ifelse_stmt:
   IF LPAREN expr RPAREN block ELSE block  { IfElse($3,$5,$7) } /* if (d > 3) { } else { } */

stmt:
    expr SEMI                             { Expr $1 } /* For any standalone expression */
  | block                                 { $1 } /*  */
  | vardecl SEMI                          { Declare($1) }
  | vardecl ASSIGN expr SEMI              { Explicit($1, $3) } /* int x = 2; */
  | ID DEFINE  expr SEMI                  { Define($1, $3) } /* id := 2 */
  | RETURN expr SEMI                      { Return $2 } /* return 2; */
  | FOR LPAREN ID IN expr RPAREN block     { Iterate($3, $5, $7) } /* for (id in array) { } or for (num in [1,2,3,4,5]) { } should be valid */
  | WHILE LPAREN expr RPAREN block        { While($3, $5) }
  | if_stmt                               { $1 }
  | ifelse_stmt                           { $1 }

