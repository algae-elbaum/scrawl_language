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

(*

let rec intermediate_of_ast (AST tree) =
    (* We'll maintain a mutating environment and pass it around by reference to
     avoid the cost of copying the hash table all the time. A deletion stack
     follows it around and records what must be deleted when leaving an 
     environment. *)
    let env = ref (Hashtbl.create 10) in
    let del = ref (Stack.create ()) in
    (fun _ -> ()) (chk_ExprList tree env del errs);
    List.rev !errs

*)
