(** This is what is run by the test executible.
    To add more tests, create a module for your tests and place inside of it
    a test_list in the same format as in eg lex_tests.ml or parse_tests.ml. Tests
    must return a bool reporting their success or failure 

   TODO 4 get a prebuild test library set up intead of this?  
 *)

(** The list of tests to be run *)
let tests = Lex_tests.test_list @ Parse_tests.test_list in

(** A function to run a given test and return a tuple of its name and return value *)
let run_test (name, test) = 
    let res = (name, test ()) in
    Printf.printf "\n";
    res
in

(* Run the tests *)
let tests_run = List.map run_test tests in

(** A function to report success or failure for a given test that's been run *)
let print_and_ret test =
    match test with
    | (name, true) -> (Printf.printf "Passed: %s\n" name; 1)
    | (name, false) -> (Printf.printf "Failed: %s\n" name; 0)
in

(* Count up how many tests passed and report it *)
let pass = List.fold_left (+) 0 (List.map print_and_ret tests_run) in

Printf.printf "Passed %d out of %d tests\n" pass (List.length tests_run);
