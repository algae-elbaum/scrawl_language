open Abstract_syntax

(** A test which parses a file and runs the simplification function on it, checking
    the simplification worked correctly *)
let simplify_while_test () = 
    Printf.printf "Parsing a file with no errors and checking that simplifying its AST works correctly\n";
    let ast = Simplifications.simplify (Scrawlc.get_ast "tests/simplify_while_test.spl") in
    let correct_ast =
        AST [DeclExpr (SimpleDecl {var_type = INT;
                                   ident = "i";
                                   pos = (0,0)});
             AssignExpr {var = SimpleVar {ident = "i";
                                          pos = (0,0)};
                         value = IntLitExpr {value = 5;
                                             pos = (0,0)}; 
                         pos = (0,0)};

             WhileExpr {cond = BinOpExpr {op = LESS;
                                        argl = VarExpr (SimpleVar {ident = "i";
                                                                   pos = (0,0)});
                                        argr= IntLitExpr {value = 10;
                                                          pos = (0,0)};
                                        pos = (0,0)};
                      body = [
                        DeclExpr (FuncDecl {func_type = 
                                                ScrawlFuncType {param_types =
                                                                    [INT; STRING];
                                                                ret_type = NONE};
                                            ident = "func";
                                            params=
                                                [QualIdent {ident_type = INT;
                                                            ident = "param1";
                                                            pos = (0,0)};
                                                 QualIdent {ident_type = STRING;
                                                            ident = "param2";
                                                            pos = (0,0)}];
                                            body = [];
                                            pos = (0,0)});
                        AssignExpr {var = SimpleVar {ident = "func";
                                                     pos = (0,0)};
                                    value = LambdaExpr {
                                                   func_type = 
                                                        ScrawlFuncType {param_types =
                                                                            [INT; STRING];
                                                                        ret_type = NONE};
                                                   params =  
                                                       [QualIdent {ident_type = INT;
                                                                   ident = "param1";
                                                                   pos = (0,0)};
                                                       QualIdent {ident_type = STRING;
                                                                  ident = "param2";
                                                                  pos = (0,0)}];
                                                   body = [ReturnExpr 
                                                            (IntLitExpr {value = 5;
                                                                       pos = (0,0)})];
                                                   pos = (0,0)};
                                    pos =(0,0)};

                        ReturnExpr (FuncCallExpr {func = "q";
                                                  args = [VarExpr (SimpleVar 
                                                                    {ident = "i";
                                                                     pos = (0,0)});
                                                          VarExpr (SimpleVar
                                                                    {ident = "j";
                                                                     pos = (0,0)})];
                                                  pos = (0,0)});

                        ReturnExpr (FuncCallExpr {func = "hiphip";
                                                  args = [VarExpr (SimpleVar 
                                                                    {ident = "i";
                                                                     pos = (0,0)});
                                                          StringLitExpr {value = "12";
                                                                         pos = (0,0)}];
                                                  pos = (0,0)});
                        AssignExpr {var = SimpleVar {ident = "i";
                                                          pos = (0,0)};
                                         value = BinOpExpr {op = PLUS;
                                                            argl = VarExpr (SimpleVar 
                                                                    {ident = "i";
                                                                     pos = (0,0)});
                                                            argr= IntLitExpr {value = 1;
                                                                             pos = (0,0)};
                                                            pos = (0,0)}; 
                                         pos = (0,0)};
                      ];
                      preface = AssignExpr {var = SimpleVar {ident = "i";
                                                             pos = (0,0)};
                                            value = IntLitExpr {value = 0;
                                                                pos = (0,0)}; 
                                            pos = (0,0)};
                      pos = (0,0)};
              WhileExpr {cond = BinOpExpr {op = LESS;
                                        argl = VarExpr (SimpleVar {ident = "j";
                                                                   pos = (0,0)});
                                        argr= IntLitExpr {value = 10;
                                                          pos = (0,0)};
                                        pos = (0,0)};
                      body = [
                        FuncCallExpr {func = "print";
                                                  args = [VarExpr (SimpleVar 
                                                                    {ident = "j";
                                                                     pos = (0,0)});];
                                                  pos = (0,0)};
                        AssignExpr {var = SimpleVar {ident = "j";
                                                          pos = (0,0)};
                                         value = BinOpExpr {op = PLUS;
                                                            argl = VarExpr (SimpleVar 
                                                                    {ident = "j";
                                                                     pos = (0,0)});
                                                            argr= IntLitExpr {value = 1;
                                                                             pos = (0,0)};
                                                            pos = (0,0)}; 
                                         pos = (0,0)};
                        
                      ];
                      preface = NoOp;
                      pos = (0,0)};
              WhileExpr {cond = NoOp;
                      body = [
                        FuncCallExpr {func = "print";
                                                  args = [VarExpr (SimpleVar 
                                                                    {ident = "z";
                                                                     pos = (0,0)});];
                                                  pos = (0,0)};
                        AssignExpr {var = SimpleVar {ident = "z";
                                                          pos = (0,0)};
                                         value = BinOpExpr {op = PLUS;
                                                            argl = VarExpr (SimpleVar 
                                                                    {ident = "z";
                                                                     pos = (0,0)});
                                                            argr= IntLitExpr {value = 1;
                                                                             pos = (0,0)};
                                                            pos = (0,0)}; 
                                         pos = (0,0)};
                        
                      ];
                      preface = NoOp;
                      pos = (0,0)};
              WhileExpr {cond = BinOpExpr {op = LESS;
                                        argl = VarExpr (SimpleVar {ident = "k";
                                                                   pos = (0,0)});
                                        argr= IntLitExpr {value = 10;
                                                          pos = (0,0)};
                                        pos = (0,0)};
                      body = [
                        FuncCallExpr {func = "print";
                                                  args = [VarExpr (SimpleVar 
                                                                    {ident = "k";
                                                                     pos = (0,0)});];
                                                  pos = (0,0)};    
                        NoOp;                    
                      ];
                      preface = NoOp;
                      pos = (0,0)};
        ] in
        (* begin *)
        (* Printf.printf "%s\n\n" (Abstract_syntax.prettyPrint_Tree(Simplifications.simplify correct_ast)); *)
    if Abstract_syntax.comp_AST ast correct_ast
        then (Printf.printf "Parsed and simplified into correct AST\n"; true)
        else (Printf.printf "Parsed and simplified into incorrect AST\n"; 
              Printf.printf "result was:\n%s\n\n" (Abstract_syntax.prettyPrint_Tree ast);
              Printf.printf "correct is:\n%s\n\n" (Abstract_syntax.prettyPrint_Tree correct_ast);
              false)
      (* end *)

(** List of functions for the test script to use as tests *)
let test_list = [("Simplify While Test", simplify_while_test);]

