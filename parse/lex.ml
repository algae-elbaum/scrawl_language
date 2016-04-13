
(* For now, let's just use ocamllex so we can get to the semantic analysis right 
   away.
   TODO 5 come back and make our own lexer and parser in the future *)

(* Get the list of tokens for a file. If there's a syntax error, raise a
   Syntax_error with the line number and how many tokens had been successfully
   read on that line *)
let tok_lst file = 
    let lexbuf = Lexing.from_channel file in
    (* Iterate through the tokens of the file, tacking them on to a list and
       keeping track of location in the file in case an error comes up *) 
    let rec next_toks c_line c_tok =
        let n_tok = Lexer.tokenize lexbuf in
        if n_tok = Parser.EOF then 
            []
        else if n_tok = Parser.SYNTAX_ERROR then
            raise (Parsing_globals.Syntax_error (c_line, c_tok))
            (* TODO 4 We can only see the first Syntax Error. Probably want to know
            about more than one error. How can we do that once the syntax
            starts being wrong? *)
        else if n_tok = Parser.EOL then 
            n_tok :: (next_toks (c_line + 1) 0)
        else 
            n_tok :: (next_toks c_line (c_tok + 1))
    in 
    next_toks 0 1



