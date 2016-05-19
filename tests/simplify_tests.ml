open Abstract_syntax

(** A test which opens a file, parses it, and compares the resulting AST with
    the expected AST. Positions are not tested *)
let simplify_while_test () = 
    Printf.printf "Parsing a file with no errors and checking its AST\n";
    let file = open_in "tests/simplify_while_test.spl" in
    let try_wrap () =
        let lexbuf = Lexing.from_channel file in
        try
            let result = Parser.main Lexer.tokenize lexbuf in
            Printf.printf "Parsing completed\n";
            close_in file;
            result
        with
            | Parsing_globals.Syntax_error (ln, ch) ->
                Printf.printf "Syntax error at line: %d, char: %d\n" ln ch;
                AST []
            | Parser.Error ->
                let ln, ch = Lexer.pos_info lexbuf in
                Printf.printf "Parsing error at line: %d, char: %d\n" ln ch;
                AST []
    in
    let ast = Simplifications.simplify (try_wrap ()) in
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
                      pos = (0,0)}
        ] in
        (* begin *)
        (* Printf.printf "%s\n\n" (Abstract_syntax.prettyPrint_Tree(Simplifications.simplify correct_ast)); *)
    if Abstract_syntax.comp_AST ast correct_ast
        then (Printf.printf "Parsed into correct AST\n"; true)
        else (Printf.printf "Parsed into incorrect AST\n"; 
              Printf.printf "result was:\n%s\n\n" (Abstract_syntax.prettyPrint_Tree ast);
              Printf.printf "correct is:\n%s\n\n" (Abstract_syntax.prettyPrint_Tree correct_ast);
              false)
      (* end *)

(** List of functions for the test script to use as tests *)
let test_list = [("Simplify While Test", simplify_while_test);]

