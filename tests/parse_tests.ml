open Abstract_syntax

(** A test which opens a file, parses it, and compares the resulting AST with
    the expected AST. Positions are not tested *)
let parse_structure_test () = 
    Printf.printf "Parsing a file with no errors and checking its AST\n";
(*     Printf.printf "Failed. Will fail until pretty printing works\n";
     false*)
   let file = open_in "tests/parse_good_structure.spl" in
(* Print out lexing so that we can compare lexed to parsed for checking *)
   (* let _ = Lex_tests.lex_test "tests/parse_good_structure.spl" in *)

    let try_wrap () =
        let lexbuf = Lexing.from_channel file in
        try
            let result = Parser.main Lexer.tokenize lexbuf in
            Printf.printf "Successfully parsed\n";
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
    let ast = try_wrap () in
      let st = Abstract_syntax.prettyPrint_Tree ast in
      begin
        Printf.printf "%s" st;
        false
      end
(*     let correct_ast =
        AST [DeclExpr (SimpleDecl {var_type = INT;
                                   ident = "i";
                                   pos = (0,0)});
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
                        DeclExpr (FuncDecl {ident = "func";
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
                                    value = LambdaExpr {params =  
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
    false
    pretty_print ast == pretty_print correct_ast *)

(** A test which opens a file that shouldn't generate any parsing errors and
    attempts to parse it *)
let parse_ok_test () =
    let filename =  "tests/parse_good.spl" in
    Printf.printf "Parsing a file with no errors. (%s)\n" filename;
    let file = open_in filename in
    let try_wrap () =
        let lexbuf = Lexing.from_channel file in
        try
            let _ = Parser.main Lexer.tokenize lexbuf in
            Printf.printf "Successfully parsed\n";
            close_in file;
            true
        with
            | Parsing_globals.Syntax_error (ln, ch) ->
                Printf.printf "Syntax error at line: %d, char: %d\n" ln ch;
                false
            | Parser.Error ->
                let ln, ch = Lexer.pos_info lexbuf in
                Printf.printf "Parsing error at line: %d, char: %d\n" ln ch;
                false
    in try_wrap ()

(** A test which opens files that should generate parsing errors and attempts to
    parse them *)
let parse_bad_test () =
    Printf.printf "Parsing files with errors.\n";
    let error_files = ["tests/parse_bad_1.spl";
                       "tests/parse_bad_2.spl";
                       "tests/parse_bad_3.spl";
                       "tests/parse_bad_4.spl";
                       "tests/parse_bad_5.spl";] in
    let error_parse f =
        let file = open_in f in
        let try_wrap () =
            let lexbuf = Lexing.from_channel file in
            try
                let _ = Parser.main Lexer.tokenize lexbuf in
                Printf.printf "Parsed %s to completion. Should have errored\n" f;
                close_in file;
                false
            with
                | Parsing_globals.Syntax_error (ln, ch) ->
                    Printf.printf "Syntax error in %s at line: %d, char: %d\n" f ln ch;
                    false
                | Parser.Error ->
                    let ln, ch = Lexer.pos_info lexbuf in
                    Printf.printf "Successfully got a parsing error";
                    Printf.printf " in %s at line: %d, char: %d\n" f ln ch;
                    true
        in try_wrap ()
    in List.fold_left (fun a b -> a && b) true (List.map error_parse error_files)



(** List of functions for the test script to use as tests *)
let test_list = [("Parse ok test", parse_ok_test);
                 ("Parse bad test", parse_bad_test);
                 ("Parser structure test", parse_structure_test);]

