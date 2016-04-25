/* Declare tokens */
%token <int * Abstract_syntax.pos> INT_LIT
%token <float * Abstract_syntax.pos> FLOAT_LIT
%token <string * Abstract_syntax.pos> STRING_LIT
%token <bool * Abstract_syntax.pos> BOOL_LIT
%token <Abstract_syntax.pos> INT_T FLOAT_T STRING_T BOOL_T
%token <Abstract_syntax.pos> BAND BOR BXOR BLEFT BRIGHT BNOT
%token <Abstract_syntax.pos> LAND LOR LNOT LXNOR LXAND LXNAND EQ LESS GREATER
%token <Abstract_syntax.pos> PLUS MINUS TIMES DIV MOD POW UMINUS
%token <Abstract_syntax.pos> ASSIGN
%token <string * Abstract_syntax.pos> IDENT
%token <Abstract_syntax.pos> IF ELSE FOR WHILE LAMBDA ARROW RETURN
%token <Abstract_syntax.pos> LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE
%token <Abstract_syntax.pos> SEMICOLON
%token <Abstract_syntax.pos> EOL EOF
%token <Abstract_syntax.pos> COMMA
/* Not quite a token, but doing it this way leads to more functional
   code for catching a syntax error when parsing a file */
%token <Abstract_syntax.pos> SYNTAX_ERROR

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
  | INT_LIT {Abstract_syntax.IntLitExpr {value=fst $1; pos=snd $1}}
  | FLOAT_LIT {Abstract_syntax.FloatLitExpr {value=fst $1; pos=snd $1}}
  | STRING_LIT {Abstract_syntax.StringLitExpr {value=fst $1; pos=snd $1}}
  | BOOL_LIT {Abstract_syntax.BoolLitExpr {value=fst $1; pos=snd $1}}
 | bin_op_expr {$1}
  | un_op_expr {$1}
  | control_flow {$1}
  | LPAREN expr RPAREN {$2}

scrawl_type:
  | simple_type {$1}
  | arr_type {$1}

simple_type:
  | INT_T {Abstract_syntax.INT}
  | FLOAT_T {Abstract_syntax.FLOAT}
  | STRING_T {Abstract_syntax.STRING}
  | BOOL_T {Abstract_syntax.BOOL}

arr_type:
  (* pos at square brace to differentiate different dimensions *)
  | scrawl_type LSQUARE INT_LIT RSQUARE 
    {Abstract_syntax.ScrawlArrayType {array_type=$1; len=fst $3; pos=$2}}

var:
  | IDENT {Abstract_syntax.SimpleVar {ident=fst $1; pos=snd $1}}  /* SimpleVar */
  (* pos at square brace to differentiate different dimensions *)
  | var LSQUARE expr RSQUARE 
    {Abstract_syntax.ArrayVar {arr=$1; idx=$3; pos=$2}} /* ArrayVar */

decl:
  | simple_type IDENT  /* SimpleDecl */
    {Abstract_syntax.SimpleDecl {var_type=$1; ident=fst $2; pos=snd $2}}
  | arr_type IDENT  /* ArrDecl */
    {Abstract_syntax.ArrDecl {arr_type=$1; ident=fst $2; pos=snd $2}}
  | scrawl_type IDENT LPAREN param_list RPAREN  /* FuncDecl */
    {Abstract_syntax.FuncDecl {ret_type=$1; ident=fst $2; params=$4; pos=snd $2}}

param_list:
  | scrawl_type IDENT COMMA param_list 
    {Abstract_syntax.QualIdent {ident_type=$1; ident=fst $2; pos=snd $2} :: $4}
  | scrawl_type IDENT 
    {[Abstract_syntax.QualIdent {ident_type=$1; ident=fst $2; pos=snd $2}]}

assign:
  | var ASSIGN expr {Abstract_syntax.AssignExpr {var=$1; value=$3; pos=$2}}

lambda:
  | LAMBDA LPAREN param_list RPAREN ARROW expr 
    {Abstract_syntax.LambdaExpr {params=$3; body=$6; pos=$1}}

bin_op_expr:
  | expr bin_op expr {Abstract_syntax.BinOpExpr {op=fst $2; argl=$1; argr=$3; pos=snd $2}}

bin_op:
    | BAND {Abstract_syntax.BAND, $1}
    | BOR {Abstract_syntax.BOR, $1}
    | BXOR {Abstract_syntax.BXOR, $1}
    | BLEFT {Abstract_syntax.BLEFT, $1}
    | BRIGHT {Abstract_syntax.BRIGHT, $1}
    | LAND {Abstract_syntax.LAND, $1}
    | LOR {Abstract_syntax.LOR, $1}
    | LXNOR {Abstract_syntax.LXNOR, $1}
    | LXAND {Abstract_syntax.LXAND, $1}
    | LXNAND {Abstract_syntax.LXNAND, $1}
    | EQ {Abstract_syntax.EQ, $1}
    | LESS {Abstract_syntax.LESS, $1}
    | GREATER {Abstract_syntax.GREATER, $1}
    | PLUS {Abstract_syntax.PLUS, $1}
    | MINUS {Abstract_syntax.MINUS, $1}
    | TIMES {Abstract_syntax.TIMES, $1}
    | DIV {Abstract_syntax.DIV, $1}
    | MOD {Abstract_syntax.MOD, $1}
    | POW {Abstract_syntax.POW, $1}

un_op_expr:
  | un_op expr {Abstract_syntax.UnOpExpr {op=fst $1; arg=$2; pos=snd $1}}

un_op:
  | BNOT {Abstract_syntax.BNOT, $1}
  | LNOT {Abstract_syntax.LNOT, $1}
  | UMINUS {Abstract_syntax.UMINUS, $1}

control_flow:
  | IF LPAREN expr RPAREN block 
    {Abstract_syntax.IfExpr {cond=$3; body=$5; else_expr = None; pos=$1}}
  | IF LPAREN expr RPAREN block ELSE block 
    {Abstract_syntax.IfExpr {cond=$3; body=$5; else_expr=$7; pos=$1}}
  | FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN block
    {Abstract_syntax.ForExpr {iter_var=$3; cond=$5; iter=$7; body=$9; pos=$1}}
  | WHILE LPAREN expr RPAREN block 
    {Abstract_syntax.WhileExpr {cond=$3; body=$5; pos=$1}}

block:
  | LCURLY expr_list RCURLY {$2}
  | expr SEMICOLON {Abstract_syntax.ExprLst ($1, None)}

expr_list:
  | expr SEMICOLON expr_list {Abstract_syntax.ExprLst ($1, $3)}
  | expr SEMICOLON {Abstract_syntax.ExprLst ($1, None)}
