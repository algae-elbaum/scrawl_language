
let tests = [Lex_tests.lex_ok_test ();
             Lex_tests.lex_bad_test ()] in

let pass = List.fold_left (+) 0 (List.map (fun b -> if b then 1 else 0) tests) in

Printf.printf "Passed %d out of %d tests\n" pass (List.length tests);

