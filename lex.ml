(* 
   For now, let's just use ocamllex so we can get to the semantic analysis right 
   away. There's a good chance we'll come back and make our own lexer and parser 
   in the future
 *)

(* Run through a file, printing each token *)
let lex_test filename =
    let f = open_in filename in 
    let line_n = ref 0 in
    let tok_n = ref 0 in
    try
        let lexbuf = Lexing.from_channel (open_in filename) in
        while true do
            let next_tok = Lexer.tokenize lexbuf in
                tok_n := !tok_n + 1;
                Printf.printf "%s " (Lexer.tokstr next_tok);
                if next_tok = Lexer.EOL then
                    (
                     Printf.printf "\n";
                     line_n := !line_n + 1;
                     tok_n  := 0;
                    );
        done
    with 
        | Lexer.Eof -> 
            close_in f;
        | Lexer.Syntax_error -> 
            close_in f;
            Printf.printf "\n\nSyntax error at line %d after token %d\n" 
                          !line_n 
                          !tok_n;
                                    
    close_in f
