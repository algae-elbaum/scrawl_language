
let parse_ok_test () =
    let lexbuf = Lexing.from_channel stdin in
    let result = Parser.main Lexer.tokenize lexbuf in
    true

let parse_bad_test () = true

let test_list = [("Parse ok test", parse_ok_test ());
                 ("Parse bad test", parse_bad_test ())]

