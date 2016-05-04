
(** Function to lex a given file into a list of the tokens that make it up. If 
    there's a syntax error, a Parsing_globals.Syntax_error is raised with the 
    line number and character of the error *)
let tok_lst filename =
    let file = open_in filename in
    let lexbuf = Lexing.from_channel file in
    (* Iterate through the tokens of the file, tacking them on to a list and
       keeping track of location in the file in case an error comes up *) 
    let rec next_toks () =
        match Lexer.tokenize lexbuf with
            | Parser.EOF _ -> []
            | Parser.SYNTAX_ERROR (ln, ch) -> 
                close_in file;
                raise (Parsing_globals.Syntax_error (ln, ch))
            (* TODO 4 We can only see the first Syntax Error. Probably want to know
            about more than one error. How can we do that once the syntax
            starts being wrong? *)
            | n_tok -> n_tok :: (next_toks ())
    in 
    let res = next_toks () in
    close_in file;
    res

    
(** Function to translate a token into a string (TODO 5 rename to string_of_tok) *)  
let tokstr = function
  | Parser.INT_LIT (i, _) -> "(INT_LIT " ^ (string_of_int i) ^ ")"
  | Parser.FLOAT_LIT (f, _) -> "(FLOAT_LIT " ^ (string_of_float f) ^ ")"
  | Parser.STRING_LIT (str, _) -> "(STRING_LIT \"" ^ str ^ "\")"
  | Parser.BOOL_LIT (b, _) -> "(BOOL_LIT " ^ (string_of_bool b) ^ ")"

  | Parser.INT_T _-> "INT_T"
  | Parser.FLOAT_T _ -> "FLOAT_T"
  | Parser.BOOL_T _ -> "BOOL_T"
  | Parser.STRING_T _ -> "STRING_T"

  | Parser.BAND _ -> "BAND"
  | Parser.BOR  _ -> "BOR"
  | Parser.BXOR _ -> "BXOR"
  | Parser.BLEFT _ -> "BLEFT"
  | Parser.BRIGHT _ -> "BRIGHT"
  | Parser.BNOT _ -> "BNOT"
  | Parser.LAND _ -> "LAND"
  | Parser.LOR _ -> "LOR"
  | Parser.LNOT _ -> "LNOT"
  | Parser.EQ _ -> "EQ"
  | Parser.LESS _ -> "LESS"
  | Parser.GREATER _ -> "GREATER"
  | Parser.PLUS _ -> "PLUS"
  | Parser.MINUS _ -> "MINUS"
  | Parser.UMINUS _ -> "UMINUS"
  | Parser.TIMES _ -> "TIMES"
  | Parser.DIV _ -> "DIV"
  | Parser.MOD _ -> "MOD"
  | Parser.POW _ -> "POW"
  | Parser.ASSIGN _ -> "ASSIGN"

  | Parser.IF _ -> "IF"
  | Parser.ELSE _ -> "ELSE"
  | Parser.FOR _ -> "FOR"
  | Parser.WHILE _ -> "WHILE"

  | Parser.IDENT (str, _) -> "(IDENT " ^ str ^ ")"

  | Parser.LPAREN _ -> "LPAREN"
  | Parser.RPAREN _ -> "RPAREN"
  | Parser.LCURLY _ -> "LCURLY"
  | Parser.RCURLY _ -> "RCURLY"
  | Parser.LSQUARE _ -> "LSQUARE"
  | Parser.RSQUARE _ -> "RSQUARE"
  | Parser.SEMICOLON _ -> "SEMICOLON"
  | Parser.COMMA _ -> "COMMA"
  | Parser.ARROW _ -> "ARROW"
  | Parser.FUNCSTART _ -> "FUNCSTART"
  | Parser.LAMBDA _ -> "LAMBDA"
  | Parser.RETURN _ -> "RETURN"
  | Parser.EOF _ -> "EOF"
  | Parser.SYNTAX_ERROR _ -> "Syntax error. An error probably should have been raised\n"

