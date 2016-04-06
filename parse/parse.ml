(* CFG
expr => Funcdec Funcbody
        Assign
        num_expr
        int_expr
        string_expr
        bool_expr
        Control Flow
        Declaration
        NULL

Assign => lvalue Assign expr

lvalue => Decl
          identifier
          identifier [ int_expr ]

Decl => Type identifier
        Type identifier [ int_expr ]

Type => *_T

Control Flow => While
                If
                For

For =>  FOR ( lvalue ; bool_expr ; expr ) block
        FOR ( Assign ; bool_expr ; expr ) block

While =>    WHILE bool_expr block

If =>   IF bool_expr block ELSE block
        IF bool_expr block

block => expr
         { expr_list }

expr_list => expr ; expr_list
             expr

num_expr => num_expr num_bop num_expr
            num_uop num_expr
            ( num_expr )
            INT_LIT
            FLOAT_LIT

int_expr => int_expr int_bop int_expr
           int_uop int_expr
           ( float_expr )
            INT_LIT

int_bop => num_bop
           MOD
           DIV
           BAND
           BOR
           BXOR
           BLEFT
           BRIGHT
           BNOT

num_bop => PLUS
           MINUS
           TIMES
           POW

string_expr => string_expr PLUS string_expr
               ( string_expr )
               STR_LIT

bool_expr => bool_expr bool_bop bool_expr
             NOT bool_expr
             ( bool_expr )
             BOOL_LIT
             string_expr comp string_expr
             num_expr comp num_expr

bool_bop => LAND
            LOR
            LNOT
            LXNOR
            LXAND
            LXNAND

comp => EQ
        LESS
        GREATER *)