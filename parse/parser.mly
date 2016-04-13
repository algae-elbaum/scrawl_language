
/* Declare tokens */
%token <string> INT_LIT
%token <string> FLOAT_LIT
%token <string> STRING_LIT
%token <string> BOOL_LIT
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
%token NULL
%token COMMA
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

/* TODO 2 include error terms so that we don't get one syntax error per compile 
oh man global error repair is cool. I think we should write that maybe*/
%start main             /* the entry point */
%type <string> main
%%
main:
  | e=expr EOL {e}

expr:
  | FUNCDEF; f=Funcbody {"FUNCDEF" ^ f}
  | a=assign {a}
  | e=num_expr {e}
  | e=int_expr {e}
  | e=string_expr {e}
  | e=bool_expr {e}
  | c=control_flow {c}
  /*| d=declaration {d} 
  What is declaration supposed to be? */
  | NULL {"NULL"}

Funcdef:
  | i=identifier; LPAREN; t=type_list; LPAREN {i^"("^t^")"}

identifier:
  | i=IDENT {i}

type_list: 
  | t=type_; q=type_list {t ^" "^q}
  | t=type_ {t}

type_:
  | INT_T {"INT_T"}
  | FLOAT_T {"FLOAT_T"}
  | STRING_T {"STRING_T"}
  | BOOL_T {"BOOL_T"}

Funcbody:
  | b=block {b}


assign:
  | l=lvalue; ASSIGN; e=expr {l ^ "="^e}

lvalue:
  | d=Decl {d}
  | i=identifier {i}
  | i=identifier; LSQUARE; e=int_expr; RSQUARE {i^"["^e^"]"}

Decl:
  | t=type_; i=identifier {t^i}
  | t=type_; i=identifier; LSQUARE; e=int_expr; RSQUARE {t^i^"["^e^"]"}

control_flow:
  | w=While {w}
  | i=If {i}
  | f=For {f}

For: 
  | FOR; LPAREN; l=lvalue; SEMICOLON; e=bool_expr; SEMICOLON; d=expr; RPAREN; b=block {"FOR("^l^";"^e^";"^d^")"^b}
  | FOR; LPAREN; l=assign; SEMICOLON; e=bool_expr; SEMICOLON; d=expr; RPAREN; b=block {"FOR("^l^";"^e^";"^d^")"^b}

While:
  | WHILE; e=bool_expr; b=block {"WHILE"^e^b}

If: 
  | IF; e=bool_expr; b=block; ELSE; c=block {"IF" ^ e ^ b ^ "ELSE" ^ c}
  | IF; e=bool_expr; b=block {"IF" ^ e ^ b}

block: 
  | e=expr {"{"^e^"}"}
  | LCURLY; e=expr_list; RCURLY {"{"^e^"}"}

expr_list:
  | e=expr; SEMICOLON; f=expr_list {e ^ ";" ^ f}
  | e=expr {e}

num_expr:
  | x = num_expr; op = num_bop; y = num_expr {op ^ x ^ y}
  | op = num_uop; x = num_expr { op ^ x }
  | LPAREN; x=num_expr; RPAREN { "("^x^")" } 
  | x = int_expr {x}
  | x = FLOAT_LIT {x}

int_expr:
  | x=int_expr; op=int_bop; y=int_expr {op ^ x ^ y}
  | op=int_uop; x=int_expr {op ^ x}
  | LPAREN; x=num_expr; RPAREN {"("^x^")"}
  | x=INT_LIT {x}

%inline int_bop:
  | x= num_bop {x}
  | MOD {"mod"}
  | DIV {"/"}
  | BAND {"land"}
  | BOR {"lor"}
  | BXOR {"lxor"}
  | BLEFT {"lsl"}
  | BRIGHT {"lsr"}
  | BNOT {"lnot"}

%inline num_uop:
  | op=int_uop {op}

%inline int_uop:
  | MINUS {"-"}
  | BNOT {"band"}

%inline num_bop:
  | PLUS {"+"}
  | MINUS {"-"}
  | TIMES {"*"}
  | POW {"**"}

string_expr:
  | x =string_expr; PLUS; y=string_expr {x ^ "concat" ^ y}
  | LPAREN; x=string_expr; RPAREN {"(x)"}
  | x=STRING_LIT {x}

bool_expr: 
  | x=bool_expr; op=bool_bop; y=bool_expr { x ^ op ^ y}
  | LNOT; x=bool_expr { "not x"}
  | LPAREN; x=bool_expr; RPAREN {"(x)"}
  | x=BOOL_LIT {x}
  | x=string_expr; op=comp; y=string_expr {x ^ op ^ y}
  | x=num_expr; op=comp; y=num_expr {x ^ op ^ y}

%inline bool_bop:
  | LAND { "and"}
  | LOR { "or"}
  | LNOT { "not" }
  | LXNOR { "xnor"}
  | LXAND { "ret false" }
  | LXNAND { "ret true" }

%inline comp:
  | EQ {"="}
  | LESS {"<"}
  | GREATER {">"}
