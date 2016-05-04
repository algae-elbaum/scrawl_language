(** A test which opens a file that shouldn't generate any lexing errors and attempts to lex it *)
let lex_ok_test () = 
    let filename = "tests/lex_test.sl" in
    Printf.printf "Lexing a file with no errors. (%s)\n" filename;
    let try_wrap () = 
        try
            (fun x -> ()) (Lex.tok_lst filename);
            Printf.printf "Successfully lexed\n";
            true
        with Parsing_globals.Syntax_error (line_n, tok_n) -> 
            Printf.printf "Syntax error at line %d after token %d\n" line_n tok_n;
            Printf.printf "Shouldn't have found an error\n";
            false
    in try_wrap ()


(* A test which opens a file that should generate a lexing error and attempts to lex it *)
let lex_bad_test () =
    let filename = "tests/lex_error_test.sl" in
    Printf.printf "Lexing a file with errors. (%s)\n" filename;
    let try_wrap () = 
        try
            (fun x -> ()) (Lex.tok_lst filename);
            Printf.printf "Lexed to completion. Should have errored\n";
            false
        with Parsing_globals.Syntax_error (line_n, tok_n) ->
            Printf.printf "Successfully errored\n";
            true
    in try_wrap ()

(** Function for use in manual testing. Prints the result of lexing a given file *)
let lex_test filename =
    try
        List.iter (fun s -> 
                         (Printf.printf "%s " (Lex.tokstr s));
                         match s with 
                           | Parser.SEMICOLON _ -> Printf.printf "\n"
                           | _ -> Printf.printf ""
                  )
                  (Lex.tok_lst filename)
    with Parsing_globals.Syntax_error (line_n, tok_n) -> 
        Printf.printf "\n\nSyntax error at line %d after token %d\n" line_n tok_n;
        raise (Parsing_globals.Syntax_error (line_n, tok_n))


(** List of functions for the test.script to use as tests *)
let test_list = [("Lex ok test", lex_ok_test);
                 ("Lex bad test", lex_bad_test)]

