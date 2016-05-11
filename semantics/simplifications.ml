
(** Translate for loops into while loops *)

let rec for_to_while tree = 
  match tree with
  | AST x -> (for_to_while_ExprList x)

and for_to_while_ExprList lst =
  List.map (fun y -> for_to_while_Expr y) lst

and for_to_while_Expr xpr=
  match xpr with
  | VarExpr var -> for_to_while_varExpr var
  | DeclExpr decl -> for_to_while_declExpr decl
  | AssignExpr {var; value; _} -> ((for_to_while_varExpr var); (for_to_while_Expr value))
  | LambdaExpr {params; body; _} -> ((for_to_while_paramList params); (for_to_while_ExprList body))
  | ReturnExpr x -> (for_to_while_Expr x)
  | FuncCallExpr {func; args; _ } -> (for_to_while_ArgList args)
  | BinOpExpr {op; argl; argr; _} -> ((for_to_while_Expr argl);  (for_to_while_Expr argr))
  | UnOpExpr {op; arg; _ } -> (for_to_while_Expr arg) 
  | IfExpr {cond; body;  else_expr; _ } -> ((for_to_while_Expr cond); (for_to_while_ExprList body); (for_to_while_ExprList else_expr))
  (** TODO this is where to change the for into a while  *)
  | ForExpr {iter_var; cond; iter; body; _ } 
        -> ((for_to_while_Expr iter_var); (for_to_while_Expr cond); (for_to_while_Expr iter); (for_to_while_ExprList body))
  | WhileExpr {cond; body; _} -> ((for_to_while_Expr cond); (for_to_while_ExprList body))
  | _ -> ()

and for_to_while_varExpr v =
  match v with
  | SimpleVar _ -> v
  | ArrayVar {arr; idx; pos} -> ArrayVar {(for_to_while_varExpr arr); (for_to_while_Expr idx); pos}

and for_to_while_declExpr d =
  match d with
  | SimpleDecl {var_type; ident; pos} -> d
  | ArrDecl {arr_type; ident; pos} -> d
  | FuncDecl {ident; params; body; pos} -> FuncDecl {ident; params; (for_to_while_ExprList body); pos}

and for_to_while_ArgList lst =
  List.map (fun y -> for_to_while_Expr y) lst

