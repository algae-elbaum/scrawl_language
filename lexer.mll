{
(*open Parser        (* The type token is defined in parser.mli *)*)
    type token =
      | INT of (int)
      | PLUS
      | MINUS
      | TIMES
      | DIV
      | POW
      | LPAREN
      | RPAREN
      | STRING of string
      | LCURLY
      | RCURLY
      | IF
      | EOL

    exception Syntax_error
    exception Eof
}

rule tokenize = parse
      [' ' '\t']        { tokenize lexbuf }     (* skip blanks *)
    | ['\n' ]           { EOL }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | '+'               { PLUS }
    | '-'               { MINUS }
    | '*'               { TIMES }
    | '/'               { DIV }
    | "**"              { POW }
    | '('               { LPAREN }
    | ')'               { RPAREN }
    | '"' (((_#'"')|("\\\""))* as str) '"' { STRING str }
    | '{'               { LCURLY }
    | '}'               { RCURLY }
    | "if"              { IF }
    (* etc *)
    | eof               { raise Eof }
    | _                 { raise Syntax_error }

{
let tokstr = function
  | INT i -> "INT" ^ (string_of_int i)
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIV -> "DIV"
  | POW -> "POW"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | STRING str -> "(STRING \"" ^ str ^ "\")"
  | LCURLY -> "LCURLY"
  | RCURLY -> "RCURLY"
  | IF -> "IF"
  | EOL -> "EOL"

}
