type intermediate_tree = INTRM_TREE of expr list

and expr =
    | CONST of int
    | NAME of label
    | TEMP of temp
    | BINOP of binop * exp * exp
    | MEM of exp
    | CALL of exp * exp list
    | ESEQ of stm * exp

and stm =
    | MOVE of exp * exp
    | EXP of exp
    | JUMP of exp * label list
    | CJUMP of relop * exp * exp * label * label
    | SEQ of stm * stm
    | LABEL of label

and binop = 
    | BAND | BOR | BXOR | BLEFT | BRIGHT 
    | LAND | LOR | LXNOR | LXAND | LXNAND 
    | PLUS | MINUS | TIMES | DIV | MOD | POW

and relop = 
    | EQ | LESS | GREATER 

(* As far as I can tell, labels and temps are only meant to be UIDs, and it's
   the responsibility of later parts of the compiler to associate them with the
   actual things in memory to which they refer *)
and label = int

and temp = int


(* I don't quite understand what the different translation phases in the Tiger
   book are talking about. They seem to be talking about some frame deal that
   they said was mutually exclusive with lambda lifting *)
