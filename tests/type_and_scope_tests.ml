open Abstract_syntax

(* Compare two errors to see if they are the same. The first is assumed to be a list
   of errors without line and character information. The second should be the list
   of errors returned by the type and scope checking function *)
let comp_errs e1 e2 =
    if (List.length e1) <> (List.length e2)
        then
            false
        else
            let comp a b =  String.length a < String.length b && a = String.sub b 0 (String.length a) in
            List.fold_left (&&) true (List.map2 comp e1 e2)

(** A test which parses a file and checks its AST for type and scope errors, 
    expecting none *)
let type_and_scope_good_test () = 
    Printf.printf "Parsing a file with no type or scope errors.\n";
    let ast = Simplifications.simplify (Scrawlc.get_ast "tests/type_and_scope_good.spl") in
    let errs = Type_and_scope_checking.chk_type_and_scope ast in
    let expected_errs = [] in
    if comp_errs expected_errs errs
        then (Printf.printf "No type or scope errors found\n"; true)
        else (Printf.printf "Found type or scope errors:\n"; 
              (fun _ -> ()) (List.map (fun s -> Printf.printf "%s\n" s)  errs);
              false)

let type_bad_test () = 
    Printf.printf "Parsing a file with type errors.\n";
    let ast = Simplifications.simplify (Scrawlc.get_ast "tests/type_bad.spl") in
    let errs = Type_and_scope_checking.chk_type_and_scope ast in
    let expected_errs = 
        ["Type float does not match type int in assignment.";
         "Type string does not match type int in assignment.";
         "Type string does not match type float in assignment.";
         "Returned type float does not match declared return type string in function definition.";
         "Type <functype: (float, string) -> string> does not match type <functype: (float, string) -> int> in assignment.";
         "Type <functype: (int, string) -> int> does not match type <functype: (float, string) -> int> in assignment.";
         "Argument does not match parameter type.";
         "Argument does not match parameter type.";
         "Not enough arguments in function call.";
         "Too many arguments in function call.";
(*         "Type int does not match type int[7][5][6] in assignment.";
         "Type int does not match type int[7][5] in assignment.";
         "Type int[5][6] does not match type int[7][5] in assignment.";
         "Type float does not match type int in assignment.";
         "Non integer index into array."; *)
         "Indexing into non array type.";
(*         "Type int does not match type int[7] in assignment."; *)
         "Condition of if statement does not have type bool.";
         "Comparison with type none.";
         "Condition of for/while statement does not have type bool.";
         "Comparison with type none.";
         "Condition of for/while statement does not have type bool.";
         "Righthand side of bitwise op does not have type int.";
         "Lefthand side of bitwise op does not have type int.";
         "Righthand side of bitwise op does not have type int.";
         "Righthand side of logical op does not have type bool.";
         "Lefthand side of logical op does not have type bool.";
         "Righthand side of logical op does not have type bool.";
         "Lefthand side of logical op does not have type bool.";
         "Righthand side of logical op does not have type bool.";
         "Righthand side of logical op does not have type bool.";
         "Comparison of unequal types.";
         "Comparison with function.";
         "Comparison with array.";
         "Comparison of unequal types.";
         "Comparison of unequal types.";
         "Type float does not match type int in assignment.";
         "Lefthand side of arithmetic op does not have arithmetic type.";
         "Lefthand side of arithmetic op does not have arithmetic type.";
         "Righthand side of arithmetic op does not have arithmetic type.";
         "Lefthand side of arithmetic op does not have arithmetic type.";
         "Operand of bitwise not does not have type int.";
         "Operand of bitwise not does not have type int.";
         "Operand of bitwise not does not have type int.";
         "Operand of bitwise not does not have type int.";
         "Operand of bitwise not does not have type int.";
         "Operand of logical not does not have type bool.";
         "Operand of logical not does not have type bool.";
         "Operand of logical not does not have type bool.";
         "Operand of logical not does not have type bool.";
         "Operand of logical not does not have type bool.";
         "Operand of logical not does not have type bool.";
         "Operand of arithmetic negation does not have arithmetic type";
         "Operand of arithmetic negation does not have arithmetic type";
         "Operand of arithmetic negation does not have arithmetic type";
         "Operand of arithmetic negation does not have arithmetic type";] in
    if comp_errs expected_errs errs
        then (Printf.printf "Found the expected errors\n"; true)
        else (Printf.printf "Found incorrect errors:\n";
              Printf.printf "Expected:\n";
              (fun _ -> ()) (List.map (fun s -> Printf.printf "%s\n" s) expected_errs);
              Printf.printf "\nFound:\n";
              (fun _ -> ()) (List.map (fun s -> Printf.printf "%s\n" s) errs);
              false)

let scope_bad_test () = 
    Printf.printf "Parsing a file with scope errors.\n";
    let ast = Simplifications.simplify (Scrawlc.get_ast "tests/scope_bad.spl") in
    let errs = Type_and_scope_checking.chk_type_and_scope ast in
    let expected_errs = 
        ["Variable a undeclared.";
         "Function yay undeclared.";
         "Variable param1 undeclared.";
         "Variable param2 undeclared.";
         "Variable param1 undeclared.";
         "Variable par2 undeclared.";
         "Variable b undeclared.";
         "Function lamb undeclared.";] in
    if comp_errs expected_errs errs
        then (Printf.printf "Found the expected errors\n"; true)
        else (Printf.printf "Found incorrect errors:\n";
              Printf.printf "Expected:\n";
              (fun _ -> ()) (List.map (fun s -> Printf.printf "%s\n" s) expected_errs);
              Printf.printf "\nFound:\n";
              (fun _ -> ()) (List.map (fun s -> Printf.printf "%s\n" s) errs);
              false)

(** List of functions for the test script to use as tests *)
let test_list = [("Type and scope ok test", type_and_scope_good_test);
                 ("Type bad test", type_bad_test);
                 ("Scope bad test", scope_bad_test);]

