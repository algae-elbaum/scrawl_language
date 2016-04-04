
(* Open a file that shouldn't generate any lexing errors and lex it *)
let lex_ok_test () = 
    Printf.printf "Lexing s file with no errors.\n";
    let file = open_in "lex_test.sc" in
    let try_wrap () = 
        try
            (fun x -> ()) (Lex.tok_list file);
            Printf.printf "Successfully lexed\n\n";
            close_in file;
            true
        with Lexer.Syntax_error (line_n, tok_n) -> 
            Printf.printf "Syntax error at line %d after token %d\n" line_n tok_n;
            Printf.printf "Shouldn't have found an error\n\n";
            close_in file;
            false
    in try_wrap ()


(* Open a file that should generate a lexing error and lex it *)
let lex_bad_test () =
    Printf.printf "Lexing a file with errors.\n";
    let file = open_in "lex_error_test.sc" in
    let try_wrap () = 
    try
        (fun x -> ()) (Lex.tok_list file);
        Printf.printf "Parsed to completion. Should have errored\n\n";
        close_in file;
        false
    with Lexer.Syntax_error (line_n, tok_n) -> 
        Printf.printf "Successfully errored\n\n";
        close_in file;
        true
    in try_wrap ()


(* Print the result of lexing a file *)
let lex_test file =
    try
        List.iter (fun s -> Printf.printf "%s " (Lexer.tokstr s)) (Lex.tok_list file)
    with Lexer.Syntax_error (line_n, tok_n) -> 
        Printf.printf "\n\nSyntax error at line %d after token %d\n" line_n tok_n;
        raise (Lexer.Syntax_error (line_n, tok_n))
