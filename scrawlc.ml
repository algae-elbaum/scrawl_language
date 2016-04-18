(* Here's the starting point of the compiler. Currently it's pretty self
   explanatory and useless *)
let () =
    if Array.length Sys.argv <> 2 then
        Printf.printf "%s" ("Usage: " ^ Sys.argv.(0) ^ " <filename>\n")
    else
    (* Open the file, tokenize it, and leave *)
    let source = open_in Sys.argv.(1) in
    let toks = Lex.tok_lst source in
    let make_something_depend_on_the_abstract_syntax_file = Abstract_syntax.INT in
    (* This is where we'd use our finished <del>trophy</del> parser. IF WE HAD ONE *)
    (* TODO 0: Run the parser and build the abstract syntax tree *)
    close_in source
