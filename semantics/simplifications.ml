open Abstract_syntax

(** Translate for loops into while loops *)

let rec simplify tree = 
    match tree with
    | AST x -> AST (simplify_ExprList x)

and simplify_ExprList lst =
    List.map (fun y -> simplify_Expr y) lst

and simplify_Expr xpr=
    match xpr with
    | VarExpr var -> VarExpr (simplify_varExpr var)
    | DeclExpr decl -> DeclExpr (simplify_declExpr decl)
    | AssignExpr {var; value; pos} ->
        AssignExpr {var = (simplify_varExpr var);
                    value = (simplify_Expr value);
                    pos}
    | LambdaExpr {func_type; params; body; pos} ->
        LambdaExpr {func_type;
                    params;
                    body = (simplify_ExprList body);
                    pos}
    | ReturnExpr x -> ReturnExpr (simplify_Expr x)
    | FuncCallExpr {func; args; pos} ->
        FuncCallExpr {func;
                      args = (simplify_ArgList args);
                      pos}
    | BinOpExpr {op; argl; argr; pos} ->
        BinOpExpr {op;
                   argl = (simplify_Expr argl);
                   argr = (simplify_Expr argr);
                   pos}
    | UnOpExpr {op; arg; pos} -> 
        UnOpExpr {op;
                  arg = (simplify_Expr arg);
                  pos}
    | IfExpr {cond; body; else_expr; pos} ->
        IfExpr {cond = (simplify_Expr cond);
                body = (simplify_ExprList body);
                else_expr = (simplify_ExprList else_expr);
                pos}
    (** Change the for into a while  *)
    (* The preface is for putting code before the loop starts just so
    we can get for loops into while loops here. *)
    | ForExpr {iter_var; cond; iter; body; pos} ->
        WhileExpr {cond = (simplify_Expr cond);
                 body =(simplify_ExprList (body @ [(simplify_Expr iter)]));
                 preface = (simplify_Expr iter_var);
                 pos}
    | WhileExpr {cond; body; preface; pos} ->
        WhileExpr {cond = (simplify_Expr cond);
                   body = (simplify_ExprList body);
                   preface = (simplify_Expr preface);
                   pos}
    (* List the rest out for explicitness' sake *)
    | NoOp
    | IntLitExpr _
    | FloatLitExpr _
    | StringLitExpr _
    | BoolLitExpr _ -> xpr

and simplify_varExpr v =
    match v with
    | SimpleVar _ -> v
    | ArrayVar {arr; idx; pos} ->
        ArrayVar {arr = (simplify_varExpr arr);
                  idx = (simplify_Expr idx);
                  pos}

and simplify_declExpr d =
    match d with
    | SimpleDecl {var_type; ident; pos} -> d
    | ArrDecl {arr_type; ident; pos} -> d
    | FuncDecl {func_type; ident; params; body; pos} ->
        FuncDecl {func_type;
                  ident;
                  params;
                  body = (simplify_ExprList body);
                  pos}

and simplify_ArgList lst =
    List.map (fun y -> simplify_Expr y) lst

