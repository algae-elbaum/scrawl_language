{
 open Lexing
 open Parser        (* The type token is defined in parser.mli *)
 let pos_info lexbuf = 
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    (line, cnum)

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let exp = ['e' 'E'] ['-' '+']? digit+
let flt = '-'? (digit+ '.' digit* | digit* '.' digit+) exp?
let white = [' ' '\t']+
let newline = ['\r' '\n']
let id = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let comment = "//" (_ # newline)* '\n'

let str_internal = ([^'"']|("\\\""))* as str
rule tokenize = parse
    | comment    { next_line lexbuf; tokenize lexbuf }
    | white      { tokenize lexbuf }     (* skip blanks *)
    | newline+   { next_line lexbuf; tokenize lexbuf }

    | flt as lxm                      { FLOAT_LIT ((float_of_string lxm), pos_info lexbuf) }
    | '-'? digit+ as lxm              { INT_LIT ((int_of_string lxm), pos_info lexbuf)}
    | '"' (str_internal as str) '"'   { STRING_LIT (str, pos_info lexbuf) }
    | "true"                          { BOOL_LIT (true, pos_info lexbuf) }
    | "false"                         { BOOL_LIT (false, pos_info lexbuf) }
    
    | "int"     { INT_T (pos_info lexbuf) }     
    | "float"   { FLOAT_T (pos_info lexbuf) }
    | "bool"    { BOOL_T (pos_info lexbuf) }
    | "string"  { STRING_T (pos_info lexbuf) }

    | "&"       { BAND (pos_info lexbuf) }
    | "|"       { BOR (pos_info lexbuf) }
    | '^'       { BXOR (pos_info lexbuf) }
    | "<<"      { BLEFT (pos_info lexbuf) }
    | ">>"      { BRIGHT (pos_info lexbuf) }
    | '~'       { BNOT (pos_info lexbuf) }
    | "&&"      { LAND (pos_info lexbuf) }
    | "and"     { LAND (pos_info lexbuf) }
    | "||"      { LOR (pos_info lexbuf) }
    | "or"      { LOR (pos_info lexbuf) }
    | '!'       { LNOT (pos_info lexbuf) }
    | "not"     { LNOT (pos_info lexbuf) }
    | "=="      { EQ (pos_info lexbuf) }
    | "is"      { EQ (pos_info lexbuf) }
    | '<'       { GREATER (pos_info lexbuf) }
    | '>'       { LESS (pos_info lexbuf) }
    | '+'       { PLUS (pos_info lexbuf) }
    | '-'       { MINUS (pos_info lexbuf) }
    | '*'       { TIMES (pos_info lexbuf) }
    | '/'       { DIV (pos_info lexbuf) }
    | '%'       { MOD (pos_info lexbuf) }
    | "**"      { POW (pos_info lexbuf) }
    | '='       { ASSIGN (pos_info lexbuf) }

    | "if"      { IF (pos_info lexbuf) }
    | "else"    { ELSE (pos_info lexbuf) }
    | "for"     { FOR (pos_info lexbuf) }
    | "while"   { WHILE (pos_info lexbuf) }

    | '('       { LPAREN (pos_info lexbuf) }
    | ')'       { RPAREN (pos_info lexbuf) }
    | '{'       { LCURLY (pos_info lexbuf) }
    | '}'       { RCURLY (pos_info lexbuf) }
    | '['       { LSQUARE (pos_info lexbuf) }
    | ']'       { RSQUARE (pos_info lexbuf) }
    | ';'       { SEMICOLON (pos_info lexbuf) }
    | ','       { COMMA (pos_info lexbuf) }
    | "->"      { ARROW (pos_info lexbuf) }
    | "fun"     { FUNCSTART (pos_info lexbuf) }
    | "lambda"  { LAMBDA (pos_info lexbuf) }
    | "return"  { RETURN (pos_info lexbuf) }
    (* id needs to have lower priority than keywords *)
    | id as ident { IDENT (ident, pos_info lexbuf) }
    | eof       { EOF (pos_info lexbuf) }
    | _         { SYNTAX_ERROR (pos_info lexbuf) }

