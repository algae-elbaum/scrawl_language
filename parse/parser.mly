/* Declare tokens */
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token <bool> BOOL_LIT
%token INT_T FLOAT_T STRING_T BOOL_T
%token BAND BOR BXOR BLEFT BRIGHT BNOT
%token LAND LOR LNOT LXNOR LXAND LXNAND EQ LESS GREATER
%token PLUS MINUS TIMES DIV MOD POW UMINUS
%token ASSIGN
%token <string> IDENT
%token IF ELSE FOR WHILE LAMBDA ARROW RETURN
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
%type <Abstract_syntax.abstract_syntax_tree> main
%%
/* TODO 0: Add pos values for the semantic values  */
main:
  | expr_list EOF {Abstract_syntax.AST $1}

/* TODO 1: Possibly distinguish expressions (things that return values) and statements
           (things that might? definitely? don't). */
expr:
  | var {VarExpr $1}
  | decl {DeclExpr $1}
  | assign {$1}
  | lambda {$1}
  | RETURN expr {$2}
  | INT_LIT {Abstract_syntax.IntLitExpr {value=$1; pos=0}}
  | FLOAT_LIT {Abstract_syntax.FloatLitExpr {value=$1; pos=0}}
  | STRING_LIT {Abstract_syntax.StringLitExpr {value=$1; pos=0}}
  | BOOL_LIT {Abstract_syntax.BoolLitExpr {value=$1; pos=0}}
  | bin_op_expr {$1}
  | un_op_expr {$1}
  | control_flow {$1}
  | LPAREN expr RPAREN {$2}

scrawl_type:
  | INT_T {Abstract_syntax.INT}
  | FLOAT_T {Abstract_syntax.FLOAT}
  | STRING_T {Abstract_syntax.STRING}
  | BOOL_T {Abstract_syntax.BOOL}
  | scrawl_type LSQUARE INT_LIT RSQUARE {Abstract_syntax.ScrawlArrayType {array_type=$1; len=$3; pos=0}}

var:
  | IDENT {Abstract_syntax.SimpleVar {ident=$1; pos=0}}  /* SimpleVar */
  | var LSQUARE expr RSQUARE {Abstract_syntax.ArrayVar {arr=$1; idx=$3; pos=0}} /* ArrayVar */

decl:
  | scrawl_type IDENT 
    {Abstract_syntax.SimpleDecl {var_type=$1; ident=$2; pos=0}} /* SimpleDecl */
  | scrawl_type IDENT LSQUARE INT_LIT RSQUARE 
    {Abstract_syntax.ArrDecl {arr_type=$1; ident=$2; len=$4; pos=0}} /* ArrDecl */
  | scrawl_type IDENT LPAREN param_list RPAREN 
    {Abstract_syntax.FuncDecl {ret_type=$1; ident=$2; params=$4; pos=0}} /* FuncDecl */

param_list:
  | scrawl_type IDENT COMMA param_list {Abstract_syntax.QualIdent {ident_type=$1; ident=$2; pos=0} :: $4}
  | scrawl_type IDENT {[Abstract_syntax.QualIdent {ident_type=$1; ident=$2; pos=0}]}

assign:
  | var ASSIGN expr {Abstract_syntax.AssignExpr {var=$1; value=$3; pos=0}}

lambda:
  | LAMBDA LPAREN param_list RPAREN ARROW expr {Abstract_syntax.LambdaExpr {params=$3; body=$6; pos=0}}

bin_op_expr:
  | expr bin_op expr {Abstract_syntax.BinOpExpr {op=$2; argl=$1; argr=$3; pos=0}}

bin_op:
    | BAND {Abstract_syntax.BAND}
    | BOR {Abstract_syntax.BOR}
    | BXOR {Abstract_syntax.BXOR}
    | BLEFT {Abstract_syntax.BLEFT}
    | BRIGHT {Abstract_syntax.BRIGHT}
    | LAND {Abstract_syntax.LAND}
    | LOR {Abstract_syntax.LOR}
    | LXNOR {Abstract_syntax.LXNOR}
    | LXAND {Abstract_syntax.LXAND}
    | LXNAND {Abstract_syntax.LXNAND}
    | EQ {Abstract_syntax.EQ}
    | LESS {Abstract_syntax.LESS}
    | GREATER {Abstract_syntax.GREATER}
    | PLUS {Abstract_syntax.PLUS}
    | MINUS {Abstract_syntax.MINUS}
    | TIMES {Abstract_syntax.TIMES}
    | DIV {Abstract_syntax.DIV}
    | MOD {Abstract_syntax.MOD}
    | POW {Abstract_syntax.POW}

un_op_expr:
  | un_op expr {Abstract_syntax.UnOpExpr {op=$1; arg=$2; pos=0}}

un_op:
  | BNOT {Abstract_syntax.BNOT}
  | LNOT {Abstract_syntax.LNOT}
  | UMINUS {Abstract_syntax.UMINUS}

control_flow:
  | IF LPAREN expr RPAREN block {Abstract_syntax.IfExpr {cond=$3; body=$5; else_expr = None; pos=0}}
  | IF LPAREN expr RPAREN block ELSE block {Abstract_syntax.IfExpr {cond=$3; body=$5; else_expr=$7; pos=0}}
  | FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN block
    {Abstract_syntax.ForExpr {iter_var=$3; cond=$5; iter=$7; body=$9; pos=0}}
  | WHILE LPAREN expr RPAREN block {Abstract_syntax.WhileExpr {cond=$3; body=$5; pos=0}}

block:
  | LCURLY expr_list RCURLY {$2}
  | expr SEMICOLON {Abstract_syntax.ExprLst ($1, None)}

expr_list:
  | expr SEMICOLON expr_list {Abstract_syntax.ExprLst ($1, $3)}
  | expr SEMICOLON {Abstract_syntax.ExprLst ($1, None)}
