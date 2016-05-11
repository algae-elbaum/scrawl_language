(** This is base code for traversal of an AST. Replace the right hand sides of
    the matches as you see fit.
    I attempted to make this a nice general function that could take as arguments
    a state and a function to apply to each node, but it turned out that the function
    would need to have almost exactly the same structure as this. All that would be
    accomplished is separating the recursion from the calculation, but that's not
    really beneficial enough, and may just straight up be bad *)

let rec traverse_Tree tree = 
  match tree with
  | AST x -> (traverse_ExprList x)

and traverse_ExprList lst =
  List.map (fun y -> traverse_Expr y) lst

and traverse_Expr xpr=
  match xpr with
  | VarExpr var -> traverse_varExpr var
  | DeclExpr decl -> traverse_declExpr decl
  | AssignExpr {var; value; _} -> ((traverse_varExpr var); (traverse_Expr value))
  | LambdaExpr {params; body; _} -> ((traverse_paramList params); (traverse_ExprList body))
  | ReturnExpr x -> (traverse_Expr x)
  | FuncCallExpr {func; args; _ } -> (traverse_ArgList args)
  | BinOpExpr {op; argl; argr; _} -> ((traverse_Expr argl);  (traverse_Expr argr))
  | UnOpExpr {op; arg; _ } -> (traverse_Expr arg) 
  | IfExpr {cond; body;  else_expr; _ } -> ((traverse_Expr cond); (traverse_ExprList body); (traverse_ExprList else_expr))
  | ForExpr {iter_var; cond; iter; body; _ } 
        -> ((traverse_Expr iter_var); (traverse_Expr cond); (traverse_Expr iter); (traverse_ExprList body))
  | WhileExpr {cond; body; _} -> ((traverse_Expr cond); (traverse_ExprList body))
  | _ -> ()

and traverse_varExpr v =
  match v with
  | SimpleVar {iden t; _} -> ()
  | ArrayVar {arr; idx; _} -> ((traverse_varExpr arr); (traverse_Expr idx))

and traverse_declExpr d =
  match d with
  | SimpleDecl {var_type; ident; _} -> (traverse_ScrawlType var_type)
  | ArrDecl {arr_type; ident; _} ->  (traverse_ScrawlType arr_type)
  | FuncDecl {ident; params; body; _} -> ((traverse_paramList params); (traverse_ExprList body))

and traverse_ScrawlType q =
  match q with
  | ScrawlArrayType {array_type; len; _} -> (traverse_ScrawlType array_type)
  | _ -> ()

and traverse_paramList p =
  List.map (fun y -> traverse_Qual y) p

and traverse_Qual q =
  match q with
  | QualIdent {ident_type; _;_} -> (traverse_ScrawlType ident_type)

and traverse_ArgList lst =
  List.map (fun y -> traverse_Expr y) lst

