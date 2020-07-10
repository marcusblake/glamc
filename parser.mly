%{
open Ast
%}

/* Need to coordinate naming for tokens */
%token SEMI 
%token LPAREN 
%token RPAREN 
%token LBRACE 
%token RBRACE 
%token LBRACKET 
%token RBRACKET 
%token PLUS 
%token MINUS 
%token MULT 
%token DIV 
%token ASSIGN 
%token DEFINE 
%token MODULUS
%token EQ 
%token NEQ 
%token LT 
%token GT 
%token LTEQ 
%token GTEQ 
%token PLEQ
%token SUBEQ
%token MLTEQ
%token DIVEQ
%token MODEQ
%token AND 
%token OR 
%token NOT
%token IF
%token ELSE
%token WHILE
%token FOR
%token IN
%token INT
%token BOOL
%token FLOAT
%token CHAR
%token STRING
%token STRUCT
%token LIST
%token RETURN
%token COMMA
%token COLON
%token DOT
%token FUNC
%token VAR
%token ELIPS
%token ARROW
%token MAKE
%token <int> LITERAL
%token <bool> BLIT
%token <char> CHARLIT /* Need to recognize chars in lexer */
%token <float> FLOATLIT /* Need to recognize floats in lexer */
%token <string> ID
%token <string> STRLIT /* Lexer needs to be able to detect strings. Need a regex for strings */
%token EOF


%start program
%type <Ast.program> program


%right NOT
%left OR
%left AND
%left EQ NEQ
%left LT LTEQ GT GTEQ
%left PLUS MINUS
%left MULT DIV MODULUS



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
  FUNC ID LPAREN func_params RPAREN return_type block
  {
    {
      func_name = $2;
      parameters = $4;
      return_type = $6;
      body = $7;
      heap_vars = []
    }
  }

return_type:
  /* NOTHING */ { Void }
  | type_       { $1 }

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

func_params:
    /* NOTHING */ { [] }
  | funcparam_comma { $1 }


funcparam_comma:
  | funcparam { [$1] }
  | funcparam COMMA funcparam_comma { $1 :: $3 }

funcparam:
  | ID COLON type_ { ($3, $1) }

type_:
  | primitive_type    { $1 }
  | aggregate_type    { $1 }
  | function_type     { $1 }

primitive_type:
  | INT               { Int }
  | BOOL              { Bool }
  | FLOAT             { Float }
  | CHAR              { Char }
  | STRING            { String } /* Need string definition in scanner */
  | ID                { Struct $1 }

aggregate_type:
  | LIST LT type_ GT  { List $3 } /* Need to define greater than in scanner */

function_type:
  | FUNC LPAREN typelist RPAREN type_  { Function ($3, $5) }

typelist:
  /* NOTHING */ { [] }
  | typelist_   { $1 }

typelist_:
  type_   { [$1] }
  | type_ COMMA typelist_ { $1 :: $3 }

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

struct_:
  | ID LBRACE struct_fields RBRACE       { StructLit($1, $3) } /* Point{x: 2, y: 3}  */

anonymous_function:
  | FUNC LPAREN func_params RPAREN return_type ARROW block 
    {
      FunctionLit({
        func_name = "";
        parameters = $3;
        return_type = $5;
        body = $7;
        heap_vars = []
      })
    }

sub_sequence:
  valexpr COLON valexpr { ($1, $3) }

value:
  | ID                                   { Id($1) } /* varName */
  | LITERAL                              { IntLit($1) } /* 2 */
  | BLIT                                 { BoolLit($1) } /* true */
  | FLOATLIT                             { FloatLit($1) } /* 3.22 */
  | CHARLIT                              { CharLit($1) } /* 'c' */
  | STRLIT                               { StringLit($1) } /*  "helloworld" */
  | value LBRACKET valexpr RBRACKET      { SeqAccess($1, $3) }
  | value LBRACKET sub_sequence RBRACKET { SubSeq($1, fst $3, snd $3) }
  | LBRACKET expr_params RBRACKET        { Seq($2) } /* [2,3,4,5] */
  | ID DOT ID                            { StructAccess($1, $3) }

boolean_expr:
  | valexpr LT valexpr                       { Binop($1, Less, $3) } 
  | valexpr GT valexpr                       { Binop($1, Greater, $3) }
  | valexpr GTEQ valexpr                     { Binop($1, Geq, $3) }
  | valexpr LTEQ valexpr                     { Binop($1, Leq, $3) }
  | valexpr AND valexpr                      { Binop($1, And, $3) } 
  | valexpr OR valexpr                       { Binop($1, Or, $3) }
  | valexpr EQ valexpr                       { Binop($1, Equal, $3) }
  | valexpr NEQ valexpr                      { Binop($1, Neq, $3) }

numerical_expr:
  | valexpr PLUS valexpr    { Binop($1, Add, $3) } 
  | valexpr MINUS valexpr   { Binop($1, Sub, $3) } 
  | valexpr MULT valexpr    { Binop($1, Mult, $3) } 
  | valexpr DIV valexpr     { Binop($1, Div, $3) } 
  | valexpr MODULUS valexpr { Binop($1, Mod, $3) } 

unary_expression:
  | MINUS valexpr         { Unop(Neg, $2) }
  | NOT valexpr           { Unop(Not, $2) }

expr:
  | valexpr                { $1 }
  | struct_                { $1 }
  | anonymous_function     { $1 }

valexpr:
  valexpr_                    { $1 }
  | LPAREN valexpr_ RPAREN    { $2 }

valexpr_:
  value                                        { $1 }
  | numerical_expr                             { $1 }
  | boolean_expr                               { $1 }
  | unary_expression                           { $1 }
  | ID LPAREN expr_params RPAREN               { Call($1, $3) }
  | MAKE LPAREN type_ COMMA expr_params RPAREN { Call("make", (TypeLit($3) :: $5)) } /* For make() builtin function */
  | ID DOT ID LPAREN expr_params RPAREN        { StructCall($1, $3, $5) } /* point.toString() */

/* CFG for defining a variable (for example int x;) */
vardecl:
    VAR ID type_ { ($3, $2) }
  /* | type_ ID ASSIGN expr { Explicit(($1, $2), $4) } */

stmt_list:
  /* nothing */     { [] }
  | stmt stmt_list  { $1 :: $2 }

block:
  LBRACE stmt_list RBRACE { Block $2 }

if_stmt:
  | IF LPAREN expr RPAREN block             { If($3,$5) }

ifelse_stmt:
   IF LPAREN expr RPAREN block ELSE block  { IfElse($3,$5,$7) } /* if (d > 3) { } else { } */

elseif_stmt:
  | IF LPAREN expr RPAREN block ELSE if_stmt { IfElse($3, $5, $7) }
  | IF LPAREN expr RPAREN block ELSE ifelse_stmt { IfElse($3, $5, $7) }

control_flow:
    FOR LPAREN ID IN expr RPAREN block               { Iterate($3, $5, $7) } /* for (id in array) { } or for (num in [1,2,3,4,5]) { } should be valid */
  | FOR LPAREN ID IN expr ELIPS expr RPAREN block    { Range($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN block                   { While($3, $5) }
  | if_stmt                                          { $1 }
  | ifelse_stmt                                      { $1 }
  | elseif_stmt                                      { $1 }

operator_eq:
  | PLEQ      { Add }
  | SUBEQ     { Sub }
  | MLTEQ     { Mult }
  | DIVEQ     { Div }
  | MODEQ     { Mod }

assignment:
    vardecl                                     { Declare($1) }
  | vardecl ASSIGN expr                         { Explicit($1, $3) } /* int x = 2; */
  | ID operator_eq expr                         { Assign($1, Binop(Id($1), $2, $3)) }
  | ID DEFINE  expr                             { Define($1, $3) } /* id := 2 */
  | ID ASSIGN expr                              { Assign($1, $3) } /* x = 2 */
  | value LBRACKET valexpr RBRACKET ASSIGN expr { AssignSeq($1, $3, $6)}
  | ID DOT ID ASSIGN expr                       { StructAssign($1, $3, $5) }

stmt:
    expr SEMI            { Expr $1 } /* For any standalone expression */
  | assignment SEMI      { $1 }
  | block                { $1 } /*  */
  | RETURN expr SEMI     { Return $2 } /* return 2; */
  | RETURN SEMI          { Return (TypeLit(Void)) }
  | control_flow         { $1 }
