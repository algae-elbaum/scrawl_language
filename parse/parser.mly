
/* Declare tokens */
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token TRUE FALSE
%token INT_T FLOAT_T STRING_T BOOL_T
%token BAND BOR BXOR BLEFT BRIGHT BNOT
%token LAND LOR LNOT LXNOR LXAND LXNAND EQ LESS GREATER
%token PLUS MINUS TIMES DIV MOD POW
%token ASSIGN
%token <string> IDENT
%token IF ELSE FOR WHILE FUNCDEF
%token LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE
%token SEMICOLON
%token EOL EOF
/* Not quite a token, but doing it this way leads to more functional
   code for catching a syntax error when parsing a file */
%token SYNTAX_ERROR

/* Set symbol precedence */
/* Lowest priority things up top */

%right IF FOR WHILE FUNCDEF
%right ELSE
%left ASSIGN 
%right INT_T FLOAT_T STRING_T BOOL_T
%left LOR
%left LAND
%left BOR
%left BXOR
%left BAND
%left EQ
%left LESS GREATER
%left BLEFT BRIGHT 
%left PLUS MINUS   
%left TIMES DIV MOD
%left POW
%right BNOT LNOT
%left SEMICOLON
%right LPAREN LSQUARE LCURLY
%left RPAREN RSQUARE RCURLY
/* Highest priority things down bottom */

%start main             /* the entry point */
%type <int> main
%%
main:
    expr EOL                { $1 }
;
expr:
    FUNCDEF Funcbody
  | ASSIGN
  | num_expr
  | int_expr
  | string_expr
  | bool_expr
  | control_flow
  | declaration
  | NULL
;

Funcdef:
    identifier ( type_list )
;

type_list: 
    type , type_list
  | type
;

Funcbody:
    block
;

Assign:
    lvalue Assign expr
;

lvalue
    Decl
  | identifier
  | identifier [ int_expr ]
;

Decl
    Type identifier
  | Type identifier [ int_expr ]
;

Type
    *_T
;

Control Flow
    While
  | If
  | For
;

For 
    FOR ( lvalue ; bool_expr ; expr ) block
  | FOR ( Assign ; bool_expr ; expr ) block
;

While
    WHILE bool_expr block
;

If 
    IF bool_expr block ELSE block
  | IF bool_expr block
;

block 
    expr
  | { expr_list }
;

expr_list
    expr ; expr_list
  | expr
;

num_expr
    num_expr num_bop num_expr
  | num_uop num_expr
  | ( num_expr )
  | INT_LIT
  | FLOAT_LIT
;

int_expr
    int_expr int_bop int_expr
  | int_uop int_expr
  | ( float_expr )
  | INT_LIT
;

int_bop 
    num_bop
  | MOD
  | DIV
  | BAND
  | BOR
  | BXOR
  | BLEFT
  | BRIGHT
  | BNOT
;

num_bop
    PLUS
  | MINUS
  | TIMES
  | POW
;

string_expr 
    string_expr PLUS string_expr
  | ( string_expr )
  | STR_LIT
;

bool_expr 
    bool_expr bool_bop bool_expr
  | NOT bool_expr
  | ( bool_expr )
  | BOOL_LIT
  | string_expr comp string_expr
  | num_expr comp num_expr
;

bool_bop
    LAND
  | LOR
  | LNOT
  | LXNOR
  | LXAND
  | LXNAND
;

comp
    EQ
  | LESS
  | GREATER
;