{
 open Parser        (* The type token is defined in parser.mli *)
}

let digit = ['0'-'9']
let character = ['A'-'z' '_']
let flt = ((digit+ '.' digit*) | (digit* '.' digit+))
let str_internal = ([^'"']|("\\\""))* as str
rule tokenize = parse
    | [' ' '\t']      { tokenize lexbuf }     (* skip blanks *)

    | digit+ as lxm                   { INT_LIT (int_of_string lxm) }
    | flt as lxm                      { FLOAT_LIT (float_of_string lxm) }
    | '"' (str_internal as str) '"'   { STRING_LIT str }
    | "true"                          { BOOL_LIT true }
    | "false"                         { BOOL_LIT false }
    
    | "int"     { INT_T }     
    | "float"   { FLOAT_T }
    | "bool"    { BOOL_T }
    | "string"  { STRING_T }

    | "&"       { BAND }
    | "|"       { BOR }
    | '^'       { BXOR }
    | "<<"      { BLEFT }
    | ">>"      { BRIGHT }
    | '~'       { BNOT }
    | "&&"      { LAND }
    | "and"     { LAND }
    | "||"      { LOR }
    | "or"      { LOR }
    | '!'       { LNOT }
    | "not"     { LNOT }
    | "=="      { EQ }
    | "is"      { EQ }
    | '<'       { GREATER }
    | '>'       { LESS }
    | '+'       { PLUS }
    | '-'       { MINUS }
    | '*'       { TIMES }
    | '/'       { DIV }
    | '%'       { MOD }
    | "**"      { POW }
    | '='       { ASSIGN }

    | "if"      { IF }
    | "else"    { ELSE }
    | "for"     { FOR }
    | "while"   { WHILE }

    | character+ as ident   { IDENT ident }

    | '('       { LPAREN }
    | ')'       { RPAREN }
    | '{'       { LCURLY }
    | '}'       { RCURLY }
    | '['       { LSQUARE }
    | ']'       { RSQUARE }
    | ';'       { SEMICOLON }
    | ','       { COMMA }
    | "->"      { ARROW }
    | "lambda"  { LAMBDA }
    | "return"  { RETURN }
    | ['\n' ]   { EOL }
    | eof       { EOF }
    | _         { SYNTAX_ERROR }

{
let tokstr = function
  | INT_LIT i -> "(INT_LIT " ^ (string_of_int i) ^ ")"
  | FLOAT_LIT f -> "(FLOAT_LIT " ^ (string_of_float f) ^ ")"
  | STRING_LIT str -> "(STRING_LIT \"" ^ str ^ "\")"
  | BOOL_LIT b-> "(BOOL_LIT " ^ (string_of_bool b) ^ ")"

  | INT_T -> "INT_T"
  | FLOAT_T -> "FLOAT_T"
  | BOOL_T -> "BOOL_T"
  | STRING_T -> "STRING_T"

  | BAND -> "BAND"
  | BOR  -> "BOR"
  | BXOR -> "BXOR"
  | BLEFT -> "BLEFT"
  | BRIGHT -> "BRIGHT"
  | BNOT -> "BNOT"
  | LAND -> "LAND"
  | LOR -> "LOR"
  | LNOT -> "LNOT"
  | LXNOR -> "LXNOR"
  | LXNAND -> "LXNAND"
  | LXAND -> "LXAND"
  | EQ -> "EQ"
  | LESS -> "LESS"
  | GREATER -> "GREATER"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | UMINUS -> "UMINUS"
  | TIMES -> "TIMES"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | POW -> "POW"
  | ASSIGN -> "ASSIGN"

  | IF -> "IF"
  | ELSE -> "ELSE"
  | FOR -> "FOR"
  | WHILE -> "WHILE"

  | IDENT str -> "(IDENT " ^ str ^ ")"

  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LCURLY -> "LCURLY"
  | RCURLY -> "RCURLY"
  | LSQUARE -> "LSQUARE"
  | RSQUARE -> "RSQUARE"
  | SEMICOLON -> "SEMICOLON"
  | COMMA -> "COMMA"
  | ARROW -> "ARROW"
  | LAMBDA -> "LAMBDA"
  | EOL -> "EOL\n"
  | EOF -> "EOF"
  | SYNTAX_ERROR -> "Syntax error. An error probably should have been raised\n"

}
