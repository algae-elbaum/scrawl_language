(** Home of the Syntax_error. I don't remember why this had to be in it's own file.
    TODO 5 We should possibly migrate this into lexer.mll of parser.mly, if only to 
    remember what the issue was with having it that way *)

exception Syntax_error of int * int
