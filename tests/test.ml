(* Basic test script. Write test functions and stick them in here if you want
   run in the test executable.
   Test functions should return a boolean indicating their success or failure.
   TODO 4 get a real test framework set up?  
 *)
let tests = Lex_tests.test_list @ Parse_tests.test_list in

let run_test (name, test) = 
    let res = (name, test ()) in
    Printf.printf "\n";
    res
in

(* Run the tests *)
let tests_run = List.map run_test tests in

let print_and_ret test =
    match test with
    | (name, true) -> (Printf.printf "Passed: %s\n" name; 1)
    | (name, false) -> (Printf.printf "Failed: %s\n" name; 0)
in

(* Count up how many tests passed and report it *)
let pass = List.fold_left (+) 0 (List.map print_and_ret tests_run) in

Printf.printf "Passed %d out of %d tests\n" pass (List.length tests_run);

