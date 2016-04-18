
/* Declare tokens */
%token <string> INT_LIT
%token <string> FLOAT_LIT
%token <string> STRING_LIT
%token <string> BOOL_LIT
%token INT_T FLOAT_T STRING_T BOOL_T
%token BAND BOR BXOR BLEFT BRIGHT BNOT
%token LAND LOR LNOT LXNOR LXAND LXNAND EQ LESS GREATER
%token PLUS MINUS TIMES DIV MOD POW UMINUS
%token ASSIGN
%token <string> IDENT
%token IF ELSE FOR WHILE LAMBDA ARROW
%token LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE
%token SEMICOLON
%token EOL EOF
%token COMMA
/* Not quite a token, but doing it this way leads to more functional
   code for catching a syntax error when parsing a file */
%token SYNTAX_ERROR

/* Set symbol precedence */
/* Lowest priority things up top */
/* COMMENTED OUT TO WORK ON GETTING OTHER STUFF TO COMPILE BETTER
   TODO 0 get this stuff in order
%right IF FOR WHILE LAMBDA
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
*/
/* Highest priority things down bottom */

/* TODO 2 include error terms so that we don't get one syntax error per compile 
oh man global error repair is cool. I think we should write that maybe*/
%start main             /* the entry point */
%type <int> main
%%
/* TODO 0: Add semantic actions for use in building the abstract syntax tree  */
main:
  | expr_list EOF {1}

expr:
  | var {}
  | decl {}
  | assign {}
  | lambda {}
  | INT_LIT {}
  | FLOAT_LIT {}
  | STRING_LIT {}
  | BOOL_LIT {}
  | bin_op_expr {}
  | un_op_expr {}
  | control_flow {}
  | LPAREN expr RPAREN {}
  | LCURLY expr_list RCURLY {}

scrawl_type:
  | INT_T {}
  | FLOAT_T {}
  | STRING_T {}
  | BOOL_T {}
  | scrawl_type LSQUARE expr RSQUARE {}

var:
  | IDENT {}  /* SimpleVar */
  | var LSQUARE expr RSQUARE {} /* ArrayVar */

decl:
  | scrawl_type IDENT {} /* SimpleDecl */
  | scrawl_type IDENT LSQUARE expr RSQUARE {} /* ArrDecl */
  | scrawl_type IDENT LPAREN param_list RPAREN {} /* FuncDecl */

param_list:
  | scrawl_type IDENT COMMA param_list {}
  | scrawl_type IDENT {}

assign:
  | var ASSIGN expr {}

lambda:
  | LAMBDA LPAREN param_list RPAREN ARROW expr {}

bin_op_expr:
  | expr bin_op expr {}

bin_op:
    | BAND {}
    | BOR {}
    | BXOR {}
    | BLEFT {}
    | BRIGHT {}
    | LAND {}
    | LOR {}
    | LXNOR {}
    | LXAND {}
    | LXNAND {}
    | EQ {}
    | LESS {}
    | GREATER {}
    | PLUS {}
    | MINUS {}
    | TIMES {}
    | DIV {}
    | MOD {}
    | POW {}

un_op_expr:
  | un_op expr {}

un_op:
  | BNOT {}
  | LNOT {}
  | UMINUS {}

control_flow:
  | IF LPAREN expr RPAREN expr {}
  | IF LPAREN expr RPAREN expr ELSE expr {}
  | FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN expr {}
  | WHILE LPAREN expr RPAREN expr {}

expr_list:
  | expr SEMICOLON expr_list {}
  | expr {}
