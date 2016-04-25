(* Open a file that shouldn't generate any lexing errors and lex it *)
let lex_ok_test () = 
    Printf.printf "Lexing a file with no errors.\n";
    let file = open_in "tests/lex_test.sc" in
    let try_wrap () = 
        try
            (fun x -> ()) (Lex.tok_lst file);
            Printf.printf "Successfully lexed\n\n";
            close_in file;
            true
        with Parsing_globals.Syntax_error (line_n, tok_n) -> 
            Printf.printf "Syntax error at line %d after token %d\n" line_n tok_n;
            Printf.printf "Shouldn't have found an error\n\n";
            close_in file;
            false
    in try_wrap ()


(* Open a file that should generate a lexing error and lex it *)
let lex_bad_test () =
    Printf.printf "Lexing a file with errors.\n";
    let file = open_in "tests/lex_error_test.sc" in
    let try_wrap () = 
        try
            (fun x -> ()) (Lex.tok_lst file);
            Printf.printf "Parsed to completion. Should have errored\n\n";
            close_in file;
            false
        with Parsing_globals.Syntax_error (line_n, tok_n) ->
            Printf.printf "Successfully errored\n\n";
            close_in file;
            true
    in try_wrap ()

(* Print the result of lexing a file *)
let lex_test file =
    try
        List.iter (fun s -> Printf.printf "%s " (Lex.tokstr s)) (Lex.tok_lst file)
    with Parsing_globals.Syntax_error (line_n, tok_n) -> 
        Printf.printf "\n\nSyntax error at line %d after token %d\n" line_n tok_n;
        raise (Parsing_globals.Syntax_error (line_n, tok_n))


let test_list = [("Lex ok test", lex_ok_test ());
                 ("Lex bad test", lex_bad_test ())]

