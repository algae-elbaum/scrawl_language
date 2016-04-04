
(* For now, let's just use ocamllex so we can get to the semantic analysis right 
   away. There's a good chance we'll come back and make our own lexer and parser 
   in the future *)

(* Get the list of tokens for a file. If there's a syntax error, raise a
   Syntax_error with the line number and how many tokens had been successfully
   read on that line *)
let tok_list file = 
    let lexbuf = Lexing.from_channel file in
    let rec next_toks c_line c_tok =
        let n_tok = Lexer.tokenize lexbuf in
        if n_tok = Lexer.EOF then 
            []
        else if n_tok = Lexer.SYNTAX_ERROR then
            raise (Lexer.Syntax_error (c_line, c_tok))
        else if n_tok = Lexer.EOL then 
            n_tok :: (next_toks (c_line + 1) 0)
        else 
            n_tok :: (next_toks c_line (c_tok + 1))
    in 
    next_toks 0 1



