
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
    INT_LIT                 { $1 }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { $1 + $3 }
  | expr MINUS expr         { $1 - $3 }
  | expr TIMES expr         { $1 * $3 }
  | expr DIV expr           { $1 / $3 }
;
