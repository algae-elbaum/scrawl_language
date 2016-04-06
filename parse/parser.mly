
/* Declare tokens */
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token TRUE FALSE
%token INT_T FLOAT_T STRING_T BOOL_T
%token BAND BOR BXOR BLEFT BRIGHT BNOT
%token LAND LOR LNOT EQ LESS GREATER
%token PLUS MINUS TIMES DIV MOD POW
%token ASSIGN
%token <string> IDENT
%token IF ELSE FOR WHILE
%token LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE
%token SEMICOLON
%token FUNCDEC
%token EOL EOF
/* Not quite a token, but doing it this way leads to more functional
   code for catching a syntax error when parsing a file */
%token SYNTAX_ERROR

/* Set symbol precedence */

/*************** 
  Left over from the example that this file came from. The current
  productions are all no good and we'll need to add a lot more and also
  figure out how precedence should work 
 ***************/
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
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
  | MINUS expr %prec UMINUS { - $2 }
;
