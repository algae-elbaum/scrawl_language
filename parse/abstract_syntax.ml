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



(** This gives a function to compare two ASTs *)
let rec comp_AST ast1 ast2 = 
  match ast1, ast2 with
  | AST x1, AST x2 -> comp_ExprList x1 x2

and comp_varExpr v1 v2 =
  match v1, v2 with
  | SimpleVar {ident = i1; _}, SimpleVar {ident = i2; _} -> i1 = i2
  | ArrayVar {arr = a1; idx = i1; _}, ArrayVar {arr = a2; idx = i2; _} -> (comp_varExpr a1 a2) && (comp_Expr i1 i2) 
  | _, _ -> false

and comp_ScrawlType q1 q2 =
  match q1, q2 with
  | INT, INT
  | FLOAT, FLOAT
  | BOOL, BOOL
  | STRING, STRING -> true
  | ScrawlArrayType {array_type = a1; len = l1; _}, ScrawlArrayType {array_type = a2; len = l2; _} -> (comp_ScrawlType a1 a2) && (l1 = l2)
  | _ -> false

and comp_Qual q1 q2 =
  match q1, q2 with
  | QualIdent {ident_type = it1; ident = i1;_}, QualIdent {ident_type = it2; ident = i2; _} -> (comp_ScrawlType it1 it2) && i1 = i2

and comp_paramList p1 p2 =
  match p1, p2 with
  | (ys1::yss1), (ys2::yss2) -> (comp_Qual ys1 ys2) && (comp_paramList yss1 yss2)
  | [], [] -> true
  | _, _ -> false

and comp_declExpr d1 d2 =
  match d1, d2 with
  | SimpleDecl {var_type = v1; ident = i1; _}, SimpleDecl {var_type = v2; ident = i2; _} -> (comp_ScrawlType v1 v2) && i1 = i2
  | ArrDecl {arr_type = a1; ident = i1; _}, ArrDecl {arr_type = a2; ident = i2; _} ->  (comp_ScrawlType a1 a2) && i1 = i2
  | FuncDecl {ident = i1; params = p1; body = b1; _}, FuncDecl {ident = i2; params = p2; body = b2; _} -> i1 = i2 && (comp_paramList p1 p2) && (comp_ExprList b1 b2)
  | _, _ -> false


and comp_BinOpExpr xpr1 xpr2 =
  match xpr1, xpr2 with
  | BAND, BAND
  | BOR, BOR
  | BXOR, BXOR
  | BLEFT, BLEFT
  | BRIGHT, BRIGHT
  | LAND, LAND
  | LOR, LOR
  | LXNOR, LXNOR
  | LXAND, LXAND
  | LXNAND, LXNAND
  | EQ, EQ
  | LESS, LESS
  | GREATER, GREATER
  | PLUS, PLUS
  | MINUS, MINUS
  | TIMES, TIMES
  | DIV, DIV
  | MOD, MOD
  | POW, POW -> true
  | _, _ -> false

and comp_UnOpExpr xpr1 xpr2 =
  match xpr1, xpr2 with
  | BNOT, BNOT
  | LNOT, LNOT
  | UMINUS, UMINUS -> true
  | _, _ -> false

and comp_Expr xpr1 xpr2 =
  match xpr1, xpr2 with
  | VarExpr var1, VarExpr var2 -> comp_varExpr var1 var2
  | DeclExpr decl1, DeclExpr decl2 -> comp_declExpr decl1 decl2
  | AssignExpr {var = vr1; value = vl1; _}, AssignExpr {var = vr2; value = vl2; _} -> (comp_varExpr vr1 vr2) && (comp_Expr vl1 vl2)
  | LambdaExpr {params = p1; body = b1; _}, LambdaExpr {params = p2; body = b2; _} -> (comp_paramList p1 p2) && (comp_ExprList b1 b2)
  | ReturnExpr x1, ReturnExpr x2 -> comp_Expr x1 x2
  | IntLitExpr {value = v1; _}, IntLitExpr {value = v2; _} -> v1 = v2
  | FloatLitExpr {value = v1; _}, FloatLitExpr {value = v2; _} -> v1 = v2
  | StringLitExpr {value = v1; _}, StringLitExpr {value = v2; _} -> v1 = v2
  | BoolLitExpr {value = v1; _}, BoolLitExpr {value = v2; _} -> v1 = v2
  | FuncCallExpr {func = f1; args = a1; _ }, FuncCallExpr {func = f2; args = a2; _ } -> f1 = f2 && (comp_ArgList a1 a2)
  | BinOpExpr {op = o1; argl = al1; argr = ar1; _}, BinOpExpr {op = o2; argl = al2; argr = ar2; _} -> (comp_Expr al1 al2) && (comp_BinOpExpr o1 o2) && (comp_Expr ar1 ar2)
  | UnOpExpr {op = o1; arg = a1; _ }, UnOpExpr {op = o2; arg = a2; _ } -> (comp_UnOpExpr o1 o2) && (comp_Expr a1 a2)
  | IfExpr {cond = c1; body = b1;  else_expr = e1; _ }, IfExpr {cond = c2; body = b2;  else_expr = e2; _ }-> (comp_Expr c1 c2) && (comp_ExprList b1 b2) && (comp_ExprList e1 e2)
  | ForExpr {iter_var = iv1; cond = c1; iter = i1; body = b1; _ }, ForExpr {iter_var = iv2; cond = c2; iter = i2; body = b2; _ }
        -> (comp_Expr iv1 iv2) && (comp_Expr c1 c2) && (comp_Expr i1 i2) && (comp_ExprList b1 b2)
  | WhileExpr {cond = c1; body = b1; _}, WhileExpr {cond = c2; body = b2; _}-> (comp_Expr c1 c2) && (comp_ExprList b1 b2)
  | _, _ -> false

and comp_ExprList lst1 lst2 =
  match lst1, lst2 with
  | (x1::xs1), (x2::xs2)-> (comp_Expr x1 x2) && (comp_ExprList xs1 xs2)
  | [], [] -> true
  | _, _ -> false

and comp_ArgList lst1 lst2 =
  match lst1, lst2 with
  | (x1::xs1), (x2::xs2) -> (comp_Expr x1 x2) && (comp_ArgList xs1 xs2)
  | [], [] -> true
  | _, _ -> false 



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
