(** A test which opens a file that shouldn't generate any parsing errors and attempts to parse it *)
let parse_ok_test () =
    Printf.printf "Parsing a file with no errors.\n";
    let file = open_in "tests/parse_test.sc" in
    let try_wrap () =
        let lexbuf = Lexing.from_channel file in
        try
            let result = Parser.main Lexer.tokenize lexbuf in
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

(** A test which opens a file that should generate parsing errors and attempts to parse it (TODO)*)
let parse_bad_test () = true

(** List of functions for the test script to use as tests *)
let test_list = [("Parse ok test", parse_ok_test);
                 ("Parse bad test", parse_bad_test)]

