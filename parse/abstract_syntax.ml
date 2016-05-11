(* Definition of the abstract syntax for scrawl. The parser builds up a tree
    of type abstract_syntax_tree as it goes through the input program and parses
    it according to the concrete syntax. The abstract syntax tree is what will be
    used in syntactic analysis *)

(** A position (line, character) type to be associated with lexing tokens for use in
    error reporting *)
type pos = int * int (* line * character *)

(** The type the tree which will be the result of parsing a file *)
(* A complete program is just a list of expressions to be sequentially evaluated *)
type abstract_syntax_tree = AST of expr list
(** An expression is any of the many basic elements of a program listed here *)
and expr = 
    | VarExpr of var
    | DeclExpr of decl
    | AssignExpr of {var : var;
                     value : expr;
                     pos : pos}

    | LambdaExpr of {params: param list;
                     body: expr list;
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
                       args: expr list;
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
                 body: expr list;
                 else_expr: expr list;
                 pos: pos}

    | ForExpr of {iter_var: expr;
                  cond: expr;
                  iter: expr;
                  body: expr list;
                  pos: pos}

    | WhileExpr of {cond: expr;
                    body: expr list;
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
                   body: expr list;
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

(* Comented out match statements are the error checking
They ar ereomved because they generate warnings. However,
this means that we know that the pretty printer will 
always work because we can't generate any syntax that won't
be printed. If changing the tree structure, consider 
reusing those checks to ensure catching any errors. *)


(* This set of functions pretty prints an AST. *)
let rec prettyPrint_varExpr v =
  match v with
  | SimpleVar {ident; _} -> ident
  | ArrayVar {arr; idx; _} -> (prettyPrint_varExpr arr) ^ (prettyPrint_Expr idx) 
  (* | _ -> "Fail VarExpr" *)

and prettyPrint_ScrawlType q =
  match q with
  | INT -> "INT "
  | FLOAT -> "FLOAT "
  | BOOL -> "BOOL "
  | STRING -> "STRING "
  | ScrawlArrayType {array_type; len; _} -> (prettyPrint_ScrawlType array_type) ^ "[" ^ (string_of_int len) ^ "]"
  (* | _ -> "Fail scrawl_type" *)

and prettyPrint_Qual q =
  match q with
  | QualIdent {ident_type; ident;_} -> (prettyPrint_ScrawlType ident_type) ^ " "^ident
  (* | _ -> "Fail QualIdent" *)

and prettyPrint_paramList p =
  match p with
  | [y] -> (prettyPrint_Qual y)
  | (ys::yss) -> (prettyPrint_Qual ys) ^ ", " ^ (prettyPrint_paramList yss)
  | [] -> "Fail paramlist"

and prettyPrint_declExpr d =
  match d with
  | SimpleDecl {var_type; ident; _} -> (prettyPrint_ScrawlType var_type) ^ ident
  | ArrDecl {arr_type; ident; _} ->  (prettyPrint_ScrawlType arr_type) ^ ident
  | FuncDecl {ident; params; body; _} -> ident ^ " " ^ (prettyPrint_paramList params) ^ "{" ^ (prettyPrint_ExprList body) ^ "}"
  (* | _ -> "Fail DeclExpr" *)


and prettyPrint_BinOpExpr xpr =
  match xpr with
  | BAND -> "BAND"
  | BOR -> "BOR"
  | BXOR ->"BXOR"
  | BLEFT -> "BLEFT"
  | BRIGHT -> "BRIGHT"
  | LAND -> "LAND"
  | LOR -> "LOR"
  | LXNOR ->"LXNOR"
  | LXAND ->"LXAND"
  | LXNAND ->"LXNAND"
  | EQ ->"EQ"
  | LESS ->"LESS"
  | GREATER ->"GREATER"
  | PLUS ->"PLUS"
  | MINUS ->"MINUS"
  | TIMES ->"TIMES"
  | DIV ->"DIV"
  | MOD ->"MOD"
  | POW ->"POW"
  (* | _ -> "Fail BinOpExpr" *)

and prettyPrint_UnOpExpr xpr =
  match xpr with
  | BNOT -> "BNOT"
  | LNOT -> "LNOT"
  | UMINUS -> "UMINUS"
  (* | _ -> "Fail UnOpExpr" *)

and prettyPrint_Expr xpr=
  match xpr with
  | VarExpr var -> prettyPrint_varExpr var
  | DeclExpr decl -> prettyPrint_declExpr decl
  | AssignExpr {var; value; _} -> (prettyPrint_varExpr var) ^ "=" ^ (prettyPrint_Expr value)
  | LambdaExpr {params; body; _} -> "lambda " ^ (prettyPrint_paramList params) ^ "{" ^(prettyPrint_ExprList body) ^ "}"
  | ReturnExpr x -> "Return " ^ (prettyPrint_Expr x)
  | IntLitExpr {value; _} -> (string_of_int value)
  | FloatLitExpr {value; _} -> (string_of_float value)
  | StringLitExpr {value; _} -> value
  | BoolLitExpr {value; _} -> (string_of_bool value)
  | FuncCallExpr {func; args; _ } -> func ^"("^ (prettyPrint_ArgList args) ^ ")" 
  | BinOpExpr {op; argl; argr; _} -> (prettyPrint_Expr argl) ^ " " ^(prettyPrint_BinOpExpr op) ^ " " ^(prettyPrint_Expr argr)
  | UnOpExpr {op; arg; _ } -> "(" ^ (prettyPrint_UnOpExpr op) ^ (prettyPrint_Expr arg) ^ ")"
  | IfExpr {cond; body;  else_expr; _ } -> "IF " ^ (prettyPrint_Expr cond) ^ "{" ^(prettyPrint_ExprList body) ^"} ELSE {" ^ (prettyPrint_ExprList else_expr) ^ "}"
  | ForExpr {iter_var; cond; iter; body; _ } -> "FOR " ^ (prettyPrint_Expr iter_var) ^ "; " ^ (prettyPrint_Expr cond) ^ "; " ^ (prettyPrint_Expr iter) ^ "{" ^ (prettyPrint_ExprList body) ^ "}"
  | WhileExpr {cond; body; _} -> "WHILE " ^ (prettyPrint_Expr cond) ^ "{" ^ (prettyPrint_ExprList body) ^ "}"
  (* | _ -> "Fail expr" *)

and prettyPrint_ExprList lst =
  match lst with
  | (x::xs)-> (prettyPrint_Expr x) ^ "\n" ^(prettyPrint_ExprList xs)
  | [] -> ""

and prettyPrint_ArgList lst =
  match lst with
  | [x] -> (prettyPrint_Expr x)
  | (x::xs)-> (prettyPrint_Expr x) ^ ", " ^(prettyPrint_ArgList xs)
  | [] -> "" 

and prettyPrint_Tree tree = 
  match tree with
  | AST (x) -> match x with
            | y -> (prettyPrint_ExprList y)
            (* | [] -> "Fail tree 2" *)
  (* | _ -> "Fail tree 1" *)