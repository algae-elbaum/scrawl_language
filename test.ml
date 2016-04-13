(* Basic test script. Write test functions and stick them in here if you want
   run in the test executable.
   Test functions should return a boolean indicating their success or failure.
   TODO 4 get a real test framework set up?  
 *)
let tests = [("Lex ok test", Lex_tests.lex_ok_test ());
             ("Lex bad test", Lex_tests.lex_bad_test ())] in

let print_and_ret test =
    match test with
    | (name, true) -> (Printf.printf "Passed: %s" name; 1)
    | (name, false) -> (Printf.printf "Failed: %s" name; 0)

(* Count up how many tests passed and report it *)
let pass = List.fold_left (+) 0 (List.map print_and_ret tests) in

Printf.printf "Passed %d out of %d tests\n" pass (List.length tests);

