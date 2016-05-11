(** The main function of the compiler *)
let () =
    if Array.length Sys.argv <> 2 then
        Printf.printf "%s" ("Usage: " ^ Sys.argv.(0) ^ " <filename>\n")
    else
    (* Tokenize the file and exit *)
    let filename = Sys.argv.(1) in
    let file = open_in filename in
    let try_wrap () =
        let lexbuf = Lexing.from_channel file in
        try
            let result = Parser.main Lexer.tokenize lexbuf in
            Printf.printf "Parsing completed\n\n";
            close_in file;
            result
        with
            | Parsing_globals.Syntax_error (ln, ch) ->
                Printf.printf "Syntax error at line: %d, char: %d\n\n" ln ch;
                Abstract_syntax.AST []
            | Parser.Error ->
                let ln, ch = Lexer.pos_info lexbuf in
                Printf.printf "Parsing error at line: %d, char: %d\\nn" ln ch;
                Abstract_syntax.AST []
    in
    let ast = try_wrap () in

    Printf.printf "Here's the pretty printed result of parsing the given file:\n";
    Printf.printf "%s\n\n" (Abstract_syntax.prettyPrint_Tree ast);

    Printf.printf "TODO more compiling"
