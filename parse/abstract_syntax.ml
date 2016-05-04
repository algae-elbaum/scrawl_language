(** Definition of the abstract syntax for scrawl. The parser builds up a tree
    of type abstract_syntax_tree as it goes through the input program and parses
    it according to the concrete syntax. The abstract syntax tree is what will be
    used in syntactic analysis *)

(** A position (line, character) type to be associated with lexing tokens for use in
    error reporting *)
type pos = int * int (* line * character *)

(** The type the tree which will be the result of parsing a file *)
(* A complete program is just a list of expressions to be sequentially evaluated *)
type abstract_syntax_tree = AST of expr_list
(** An expression is any of the many basic elements of a program listed here *)
and expr = 
    | VarExpr of var
    | DeclExpr of decl
    | AssignExpr of {var : var;
                     value : expr;
                     pos : pos}

    | LambdaExpr of {params: param list;
                     body: expr_list;
                     pos: pos}

    | ReturnExpr of expr

    | IntLitExpr of {value: int;
                     pos: pos}

    | FloatLitExpr of {value: float;
                       pos: pos}

    | StringLitExpr of {value: string;
                        pos: pos}
                        
    | BoolLitExpr of {value: bool;
                      pos: pos}

    | FuncCallExpr of {func: string;
                       args: expr_list;
                       pos: pos}

    | BinOpExpr of {op: bin_op;
                    argl: expr;
                    argr: expr;
                    pos: pos}

    | UnOpExpr of {op: un_op;
                   arg: expr;
                   pos: pos}

    (* Control flow is C style *)
    | IfExpr of {cond: expr;
                 body: expr_list;
                 else_expr: expr_list;
                 pos: pos}

    | ForExpr of {iter_var: expr;
                  cond: expr;
                  iter: expr;
                  body: expr_list;
                  pos: pos}

    | WhileExpr of {cond: expr;
                    body: expr_list;
                    pos: pos}

and var = 
    | SimpleVar of {ident: string;
                    pos: pos}

    | ArrayVar of {arr: var;
                   idx: expr;
                   pos: pos}
and decl = 
    | SimpleDecl of {var_type: scrawl_type;
                     ident: string;
                     pos: pos}

    | ArrDecl of {arr_type: scrawl_type;
                  ident: string;
                  pos: pos}

    | FuncDecl of {ident: string;
                   params: param list;
                   body: expr_list;
                   pos: pos}

and bin_op = 
    | BAND | BOR | BXOR | BLEFT | BRIGHT 
    | LAND | LOR | LXNOR | LXAND | LXNAND 
    | EQ | LESS | GREATER 
    | PLUS | MINUS | TIMES | DIV | MOD | POW

and un_op = 
    | BNOT | LNOT | UMINUS

and scrawl_type =
    | INT | FLOAT | BOOL | STRING 
    | ScrawlArrayType of {array_type: scrawl_type;
                          len: int;
                          pos: pos}

and param = QualIdent of {ident_type: scrawl_type;
                          ident: string;
                          pos: pos}

and expr_list = expr list

