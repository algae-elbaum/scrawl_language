
let parse_ok_test () =
    Printf.printf "Parsing a file with no errors.\n";
    let file = open_in "tests/parse_test.sc" in
    let try_wrap () =
        try
            let lexbuf = Lexing.from_channel file in
            let result = Parser.main Lexer.tokenize lexbuf in
            Printf.printf "Successfully parsed\n\n";
            close_in file;
            true
        with 
            | _ -> (* TODO 0: get error reporting *)
                  Printf.printf "Got an error, no parsing error reporting yet\n\n";
                  close_in file;
                  false
    in try_wrap ()

let parse_bad_test () = true

let test_list = [("Parse ok test", parse_ok_test ());
                 ("Parse bad test", parse_bad_test ())]

