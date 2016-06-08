%{
    let types_of_params params =
       List.map (fun (Abstract_syntax.QualIdent {ident_type; ident; _}) -> ident_type)
                params 
%}


(* Declare tokens *)
%token <int * Abstract_syntax.pos> INT_LIT
%token <float * Abstract_syntax.pos> FLOAT_LIT
%token <string * Abstract_syntax.pos> STRING_LIT
%token <bool * Abstract_syntax.pos> BOOL_LIT
%token <Abstract_syntax.pos> INT_T FLOAT_T STRING_T BOOL_T NONE_T
%token <Abstract_syntax.pos> BAND BOR BXOR BLEFT BRIGHT BNOT
%token <Abstract_syntax.pos> LAND LOR LNOT EQ LESS GREATER
%token <Abstract_syntax.pos> PLUS MINUS TIMES DIV MOD POW UMINUS
%token <Abstract_syntax.pos> ASSIGN
%token <string * Abstract_syntax.pos> IDENT
%token <Abstract_syntax.pos> IF ELSE FOR WHILE ARROW RETURN
%token <Abstract_syntax.pos> LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE
%token <Abstract_syntax.pos> SEMICOLON
%token <Abstract_syntax.pos> EOF
%token <Abstract_syntax.pos> COMMA
%token <Abstract_syntax.pos> FUNCSTART
(* Not quite a token, but doing it this way leads to more functional
   code for catching a syntax error when parsing a file *)
%token <Abstract_syntax.pos> SYNTAX_ERROR

(* Set symbol precedence *)
(* Lowest priority things up top *)
%right RETURN

(* %right IF
%nonassoc THEN
%right ELSE *)

%left ASSIGN 
%left LOR
%left LAND
%left BOR
%left BXOR
%left BAND
%left EQ
%left LESS GREATER
%left PLUS MINUS   
%left TIMES DIV MOD
%right UMINUS
%left BLEFT BRIGHT 
%left POW
(* Highest priority things down bottom *)

(* TODO 2 include error terms so that we don't get one syntax error per compile 
oh man global error repair is cool. I think we should write that maybe*)
%start main             (* the entry point *)
%type <Abstract_syntax.abstract_syntax_tree> main
%%
main:
  | expr_list EOF {Abstract_syntax.AST $1}

(* TODO 3: Possibly distinguish expressions (things that return values) and statements
           (things that might? definitely? don't). *)
expr:
  | var {Abstract_syntax.VarExpr $1}
  | decl {Abstract_syntax.DeclExpr $1}
  | assign {$1}
  | lambda {$1}
  | IDENT LPAREN arg_list RPAREN {Abstract_syntax.FuncCallExpr 
      {func= fst $1; args=$3; pos=snd $1}}
  | RETURN expr {Abstract_syntax.ReturnExpr $2}
  | INT_LIT {Abstract_syntax.IntLitExpr {value=fst $1; pos=snd $1}}
  | FLOAT_LIT {Abstract_syntax.FloatLitExpr {value=fst $1; pos=snd $1}}
  | STRING_LIT {Abstract_syntax.StringLitExpr {value=fst $1; pos=snd $1}}
  | BOOL_LIT {Abstract_syntax.BoolLitExpr {value=fst $1; pos=snd $1}}
  | bin_op_expr {$1}
  | un_op_expr {$1}
  | control_flow {$1}
  | LPAREN expr RPAREN {$2}
  (* This is only to keep the compiler from warning at us: *)
  | SYNTAX_ERROR {raise (Parsing_globals.Syntax_error (fst $1, snd $1))}


scrawl_type:
  | simple_type {$1}
  | arr_type {$1}

simple_type:
  | INT_T {Abstract_syntax.INT}
  | FLOAT_T {Abstract_syntax.FLOAT}
  | STRING_T {Abstract_syntax.STRING}
  | BOOL_T {Abstract_syntax.BOOL}
  | NONE_T {Abstract_syntax.NONE}

arr_type:
  (* pos at square brace to differentiate different dimensions *)
  | simple_type LSQUARE INT_LIT RSQUARE
    {Abstract_syntax.ScrawlArrayType {array_type=$1; len=fst $3; pos=$2}}

var:
  | IDENT {Abstract_syntax.SimpleVar {ident=fst $1; pos=snd $1}}  (* SimpleVar *)
  (* pos at square brace to differentiate different dimensions *)
  | IDENT LSQUARE expr RSQUARE 
    {Abstract_syntax.ArrayVar {arr=fst $1; idx=$3; pos=$2}} (* ArrayVar *)

decl:
  | simple_type IDENT  (* SimpleDecl *)
    {Abstract_syntax.SimpleDecl {var_type=$1; ident=fst $2; pos=snd $2}}
  | arr_type IDENT (* ArrDecl *)
    {Abstract_syntax.ArrDecl {arr_type=$1; ident=fst $2; pos=snd $2}}
  | func_decl {$1} 

func_decl:
  | FUNCSTART IDENT LPAREN param_list RPAREN ARROW scrawl_type
    {Abstract_syntax.FuncDecl {func_type=Abstract_syntax.ScrawlFuncType 
                                            {param_types=types_of_params $4;
                                             ret_type=$7}; 
                               ident=fst $2; params=$4; body=[]; pos=$1}}
  (* We count definition at the time of declaration as part of declaration *)
  | FUNCSTART IDENT LPAREN param_list RPAREN ARROW scrawl_type block
    {Abstract_syntax.FuncDecl {func_type=Abstract_syntax.ScrawlFuncType
                                            {param_types=types_of_params $4;
                                             ret_type=$7}; 
                               ident=fst $2; params=$4; body=$8; pos=$1}}

(* This is for declaring functions *)
param_list:
  | scrawl_type IDENT COMMA param_list
    {Abstract_syntax.QualIdent {ident_type=$1; ident=fst $2; pos=snd $2} :: $4}
  | scrawl_type IDENT
    {[Abstract_syntax.QualIdent {ident_type=$1; ident=fst $2; pos=snd $2}]}
   | {[]}

assign:
  | var ASSIGN expr {Abstract_syntax.AssignExpr {var=$1; value=$3; pos=$2}}

lambda:
  | FUNCSTART LPAREN param_list RPAREN ARROW scrawl_type block 
    {Abstract_syntax.LambdaExpr {func_type=Abstract_syntax.ScrawlFuncType
                                            {param_types=types_of_params $3;
                                            ret_type=$6};
                                 params=$3; body=$7; pos=$1}}

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
    | un_op expr %prec UMINUS {Abstract_syntax.UnOpExpr {op=fst $1; arg=$2; pos=snd $1}}

un_op:
    | BNOT {Abstract_syntax.BNOT, $1}
    | LNOT {Abstract_syntax.LNOT, $1}
    | MINUS {Abstract_syntax.UMINUS, $1}

control_flow:
    | IF LPAREN expr RPAREN block 
        {Abstract_syntax.IfExpr {cond=$3; body=$5; else_expr = []; pos=$1}}
    | IF LPAREN expr RPAREN block ELSE block 
        {Abstract_syntax.IfExpr {cond=$3; body=$5; else_expr=$7; pos=$1}}
    (* It's permissible to leave out any of the three exprs here:  *)
    | FOR LPAREN opt_expr SEMICOLON opt_expr SEMICOLON opt_expr RPAREN block
        {Abstract_syntax.ForExpr {iter_var=$3; cond=$5; iter=$7; body=$9; pos=$1}}
    | WHILE LPAREN expr RPAREN block 
        {Abstract_syntax.WhileExpr {cond=$3;
                                    body=$5; 
                                    preface = Abstract_syntax.NoOp; 
                                    pos=$1}}

opt_expr:
  | expr {$1}
  |      {Abstract_syntax.NoOp}

block:
  | LCURLY expr_list RCURLY {$2}

expr_list:
  | expr SEMICOLON expr_list {$1 :: $3}
  |  {[]}

(* This is for calling functions*)
arg_list:
  | expr COMMA arg_list {$1 :: $3}
  | expr {[$1]}
  |  {[]}
