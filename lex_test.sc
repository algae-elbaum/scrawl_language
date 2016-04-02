    | [\t]      { tokenize lexbuf }     (* skip blanks *)

    | digit+ as lxm                   { INT_LIT (int_of_string lxm) }
    | flt as lxm                      { FLOAT_LIT (float_of_string lxm) }
    | " (str_internal as str) "   { STRING_LIT str }
    | "true"                          { TRUE }
    | "false"                         { FALSE }
    
    | "int"     { INT_T }     
    | "float"   { FLOAT_T }
    | "bool"    { BOOL_T }
    | "string"  { STRING_T }

    | "&"       { BAND }
    | "|"       { BOR }
    | ^       { BXOR }
    | "<<"      { BLEFT }
    | ">>"      { BRIGHT }
    | ~       { BNOT }
    | "&&"      { LAND }
    | "and"     { LAND }
    | "||"      { LOR }
    | "or"      { LOR }
    | !       { LNOT }
    | "not"     { LNOT }
    | "=="      { EQ }
    | "is"      { EQ }
    | <       { GREATER }
    | >       { LESS }
    | +       { PLUS }
    | -       { MINUS }
    | *      { TIMES }
    | /      { DIV }
    | %       { MOD }
    | "**"      { POW }
    | =       { ASSIGN }

    | "if"      { IF }
    | "else"    { ELSE }
    | "for"     { FOR }
    | "while"   { WHILE }

    | character+ as ident   { IDENT ident }

    | (       { LPAREN }
    | )       { RPAREN }
    | {       { LCURLY }
    | }       { RCURLY }
    | ;       { SEMICOLON }
    | "func"    { FUNCDEC }
    | [n ]   { EOL }
    ( etc )
    | eof       { raise Eof }
    | _         { raise Syntax_error }


