(** The function(s) in this file will verify that a given AST has correct scoping
    and types *)
    
(* Not clear yet whether this should be one function or two *)
(* TODO 1: Complete this function *)
(* TODO 0: I think we should first run through some AST simplifications
           (eg turn all for loops into while loops, separating simultaneous
           declarations and initializations). It'll make this and subsequent
           processing easier, and it doesn't depend on type or scope correctness *)
let rec check_type_and_scope tree =
  (* We'll maintain a mutating environment and pass it around by reference to
     avoid the cost of copying the hash table all the time. A deletion stack
     follows it around and records what must be deleted when leaving an 
     environment *)
  let env = ref (Hashtbl.create 10) in
  let del_stack = ref (Stack.create ()) in
  match tree with
  | AST x -> (check_ExprList x)

and check_ExprList lst =
  List.map (fun y -> check_Expr y) lst

and check_Expr xpr=
  match xpr with
  | VarExpr var -> check_varExpr var
  | DeclExpr decl -> check_declExpr decl
  | AssignExpr {var; value; _} -> ((check_varExpr var); (check_Expr value))
  | LambdaExpr {params; body; _} -> ((check_paramList params); (check_ExprList body))
  | ReturnExpr x -> (check_Expr x)
  | FuncCallExpr {func; args; _ } -> (check_ArgList args)
  | BinOpExpr {op; argl; argr; _} -> ((check_Expr argl);  (check_Expr argr))
  | UnOpExpr {op; arg; _ } -> (check_Expr arg) 
  | IfExpr {cond; body;  else_expr; _ } -> ((check_Expr cond); (check_ExprList body); (check_ExprList else_expr))
  | ForExpr {iter_var; cond; iter; body; _ } 
        -> ((check_Expr iter_var); (check_Expr cond); (check_Expr iter); (check_ExprList body))
  | WhileExpr {cond; body; _} -> ((check_Expr cond); (check_ExprList body))
  | _ -> ()

and check_varExpr v =
  match v with
  | SimpleVar {iden t; _} -> ()
  | ArrayVar {arr; idx; _} -> ((check_varExpr arr); (check_Expr idx))

and check_declExpr d =
  match d with
  | SimpleDecl {var_type; ident; _} -> (check_ScrawlType var_type)
  | ArrDecl {arr_type; ident; _} ->  (check_ScrawlType arr_type)
  | FuncDecl {ident; params; body; _} -> ((check_paramList params); (check_ExprList body))

and check_ScrawlType q =
  match q with
  | ScrawlArrayType {array_type; len; _} -> (check_ScrawlType array_type)
  | _ -> ()

and check_paramList p =
  List.map (fun y -> check_Qual y) p

and check_Qual q =
  match q with
  | QualIdent {ident_type; _;_} -> (check_ScrawlType ident_type)

and check_ArgList lst =
  List.map (fun y -> check_Expr y) lst

