
let () =
    if Array.length Sys.argv <> 2 then
        Printf.printf "%s" ("Usage: " ^ Sys.argv.(0) ^ " <filename>\n")
    else
    
    let source = open_in Sys.argv.(1) in
    let toks = Lex.tok_lst source in
    close_in source
