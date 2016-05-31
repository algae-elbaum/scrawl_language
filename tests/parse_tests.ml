open Abstract_syntax

(** A test which opens a file, parses it, and compares the resulting AST with
    the expected AST. Positions are not tested *)
let parse_structure_test () = 
    Printf.printf "Parsing a file with no errors and checking its AST\n";
    let ast = Scrawlc.get_ast "tests/parse_good_structure.spl" in
    let correct_ast =
        AST [DeclExpr (SimpleDecl {var_type = INT;
                                   ident = "i";
                                   pos = (0,0)});
             AssignExpr {var = SimpleVar {ident = "i";
                                          pos = (0,0)};
                         value = IntLitExpr {value = 5;
                                             pos = (0,0)}; 
                         pos = (0,0)};

             ForExpr {iter_var = AssignExpr {var = SimpleVar {ident = "i";
                                                              pos = (0,0)};
                                             value = IntLitExpr {value = 0;
                                                                 pos = (0,0)}; 
                                             pos = (0,0)};
                      cond = BinOpExpr {op = LESS;
                                        argl = VarExpr (SimpleVar {ident = "i";
                                                                   pos = (0,0)});
                                        argr= IntLitExpr {value = 10;
                                                          pos = (0,0)};
                                        pos = (0,0)};
                      iter = AssignExpr {var = SimpleVar {ident = "i";
                                                          pos = (0,0)};
                                         value = BinOpExpr {op = PLUS;
                                                            argl = VarExpr (SimpleVar 
                                                                    {ident = "i";
                                                                     pos = (0,0)});
                                                            argr= IntLitExpr {value = 1;
                                                                             pos = (0,0)};
                                                            pos = (0,0)}; 
                                         pos = (0,0)};
                      body = [
                        DeclExpr (FuncDecl {func_type = 
                                                ScrawlFuncType {param_types =
                                                                    [INT; STRING];
                                                                ret_type = INT};
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
                                                                        ret_type = INT};
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
                                                  pos = (0,0)})
                      ];
                      pos = (0,0)}
        ] in
        
    if Abstract_syntax.comp_AST ast correct_ast
        then (Printf.printf "Parsed into correct AST\n"; true)
        else (Printf.printf "Parsed into incorrect AST\n"; 
              Printf.printf "result was:\n%s\n\n" (Abstract_syntax.prettyPrint_Tree ast);
              Printf.printf "correct is:\n%s\n\n" (Abstract_syntax.prettyPrint_Tree correct_ast);
              false)

(** A test which opens a file that shouldn't generate any parsing errors and
    attempts to parse it *)
let parse_ok_test () =
    let filename =  "tests/parse_good.spl" in
    Printf.printf "Parsing a file with no errors. (%s)\n" filename;
    (Scrawlc.get_ast filename) <> AST []

(** A test which opens files that should generate parsing errors and attempts to
    parse them *)
let parse_bad_test () =
    Printf.printf "Parsing files with errors.\n";
    let error_files = ["tests/parse_bad_1.spl";
                       "tests/parse_bad_2.spl";
                       "tests/parse_bad_3.spl";
                       "tests/parse_bad_4.spl";
                       "tests/parse_bad_5.spl";] in
    let error_parse f = (Scrawlc.get_ast f) = AST []
    in List.fold_left (fun a b -> a && b) true (List.map error_parse error_files)



(** List of functions for the test script to use as tests *)
let test_list = [("Parse ok test", parse_ok_test);
                 ("Parse bad test", parse_bad_test);
                 ("Parser structure test", parse_structure_test);]

