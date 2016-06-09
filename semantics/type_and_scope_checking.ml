open Abstract_syntax

(** The function(s) in this file will verify that a given AST has correct scoping
    and types *)

let pos_string (ln, ch) =
    " Line " ^ (string_of_int ln) ^ ", character " ^ (string_of_int ch)

let add_ident ident ident_type env del =
    Hashtbl.add !env ident ident_type;
    Stack.push ident !del

(** Rewind the environment to immediately before it entered the current level of
    scoping. Raises Empty when at global scope. (So don't use it at global scope.
    There should be no reason to do so) *)
let rec rewind_env env del =
    match (Stack.pop !del) with
    | "*" -> () (* A star denotes the beginning of a new scope *)
    | s -> begin
           Hashtbl.remove !env s;
           rewind_env env del;
           end

(** Take an AST and make sure that variable accesses are always to variables 
    that are in scope and that arguments always match the type of the function
    parameters. Returns a list of error strings *)
let rec chk_type_and_scope (AST tree) =
    (* We'll maintain a mutating environment and pass it around by reference to
     avoid the cost of copying the hash table all the time. A deletion stack
     follows it around and records what must be deleted when leaving an 
     environment. A string also follows it around, and whenever there is a
     scope or type error, an appropriate error message is appended *)
    let env = ref (Hashtbl.create 10) in
    let del = ref (Stack.create ()) in
    let errs = ref [] in
    (fun _ -> ()) (chk_ExprList tree env del errs);
    List.rev !errs

(* The return value of each of these functions is the type of the expression the
   function was called on. *)
and chk_ExprList lst env del errs =
    (* First simply check every expression *)
    let types = (List.map (fun x -> chk_Expr x env del errs) lst) in
    (* Pair each expression with its type *)
    let zipped = List.mapi (fun idx x -> (x, List.nth types idx)) lst in
    (* Then extract the return expressions and check that they agree *)
    let returns = List.filter (fun (x, _) -> match x with | ReturnExpr _ -> true | _ -> false)
                              zipped in
    
    let rec check_type_equality pair_lst des_type =
        match pair_lst with
        | [] -> true
        | (_, curr_type) :: t -> curr_type = des_type && check_type_equality t des_type
    in
    let des_type = 
        if returns = [] 
            then NONE
            else snd (List.hd returns)
    in
    if check_type_equality returns des_type
        then des_type
        else 
            begin
            let pos = extract_pos (fst (List.hd returns)) in
            errs := ("Return statements in function definition do not agree on return type."
                    ^ (pos_string pos)) :: !errs;
            NONE
            end

and chk_Expr xpr env del errs =
    match xpr with
    | VarExpr var -> chk_varExpr var env del errs
    | DeclExpr decl -> chk_declExpr decl env del errs
    | AssignExpr {var; value; pos} -> 
        let (t1, t2) = ((chk_varExpr var env del errs), (chk_Expr value env del errs)) in
        (* If t1 is none that means the lhs was out of scope, so reporting a type error
           would be redundant. *)
        if t1 = NONE
            then
                t2
            else
                begin
                if not (comp_ScrawlType t1 t2) && (t1 <> FLOAT || t2 <> INT)
                    then errs := ("Type " ^ (string_of_type t2) ^ " does not match type " 
                                  ^ (string_of_type t1) ^ " in assignment."
                                  ^ (pos_string pos)) :: !errs;
                t1
                end
    | LambdaExpr {func_type; params; body; pos} -> 
        begin
        Stack.push "*" !del; (* Start a new scope *)
        chk_paramList params env del errs; (* Adds each param to the env *)
        let returned = chk_ExprList body env del errs in
        let ret_type = match func_type with
            | ScrawlFuncType {param_types; ret_type} -> ret_type
            | _ -> raise (Invalid_argument "Type of lambda is not function type. (Compiler bug)")
        in
        if (ret_type <> returned)
            then
                errs := ("Returned type " ^ (string_of_type returned) 
                         ^ " does not match declared return type " 
                         ^  (string_of_type ret_type) 
                         ^ " in function definition." ^ (pos_string pos)) :: !errs;
        rewind_env env del;
        func_type
        end
    | ReturnExpr x -> chk_Expr x env del errs
    | PrintExpr _ -> NONE
    | IntLitExpr _ -> INT
    | FloatLitExpr _ -> FLOAT
    | StringLitExpr _ -> STRING
    | BoolLitExpr _ -> BOOL
    | FuncCallExpr {func; args; pos} ->
        if Hashtbl.mem !env func
            then
                let t = Hashtbl.find !env func in
                match t with
                | ScrawlFuncType {param_types; ret_type} ->
                    begin
                    chk_argList args param_types pos env del errs;
                    ret_type
                    end
                | _ -> 
                    begin
                    errs := (func ^ " does not have function type." 
                            ^ (pos_string pos)) :: !errs;
                    NONE
                    end
            else
                begin
                errs := ("Function " ^ func ^ " undeclared." ^ (pos_string pos)) :: !errs;
                NONE
                end

    | BinOpExpr {op; argl; argr; pos} -> 
        begin
        let t1 = chk_Expr argl env del errs in
        let t2 = chk_Expr argr env del errs in
        match op with
        | BAND
        | BOR
        | BXOR
        | BLEFT
        | BRIGHT -> begin
                    chk_bitwise_op t1 t2 pos errs;
                    INT
                    end
        | LAND
        | LOR
        | LXNOR
        | LXAND
        | LXNAND -> begin
                    chk_logical_op t1 t2 pos errs;
                    BOOL
                    end
        | EQ
        | LESS
        | GREATER -> begin
                     chk_comp_op t1 t2 pos errs;
                     BOOL
                     end
        | PLUS
        | MINUS
        | TIMES
        | DIV
        | MOD
        | POW -> chk_arith_op t1 t2 pos errs
        end

    | UnOpExpr {op; arg; pos} ->
        begin
        let arg_type = chk_Expr arg env del errs in
        match op with 
        | BNOT ->
            begin 
            if arg_type <> INT
                then 
                    errs := ("Operand of bitwise not does not have type int." 
                            ^ (pos_string pos)) :: !errs
            end;
            INT
        | LNOT ->
            if arg_type <> BOOL
                then errs := ("Operand of logical not does not have type bool."
                             ^ (pos_string pos)) :: !errs;
            BOOL
        | UMINUS ->
            if arg_type <> INT && arg_type <> FLOAT
                then begin
                     errs := ("Operand of arithmetic negation does not have arithmetic type."
                             ^ (pos_string pos)) :: !errs;
                     INT
                     end
                else arg_type
        end

    | IfExpr {cond; body;  else_expr; pos} -> 
        begin
        if (chk_Expr cond env del errs) <> BOOL
            then errs := ("Condition of if statement does not have type bool."
                          ^ (pos_string pos)) :: !errs;
        Stack.push "*" !del; (* Start a new scope *)
        (fun _ -> ()) (chk_ExprList body env del errs);
        rewind_env env del;
        Stack.push "*" !del; (* Start a new scope *)
        let ret =(chk_ExprList else_expr env del errs) in
        rewind_env env del;
        ret
        end
    | ForExpr _  -> 
        raise (Invalid_argument "Type/scope checker got AST with for loop (compiler bug)")
    | WhileExpr {cond; body; preface; pos} ->
        begin
        Stack.push "*" !del; (* Start a new scope *)
        (fun _ -> ()) (chk_Expr preface env del errs);
        if (chk_Expr cond env del errs) <> BOOL
            then errs := ("Condition of for/while statement does not have type bool."
                          ^ (pos_string pos)) :: !errs;
        let ret = (chk_ExprList body env del errs) in
        rewind_env env del;
        ret
        end
    | NoOp -> NONE

(* Any appearence of a var must have the var already in scope. Anything that
   adds a symbol to the scope is not a var. Those will be decls and params *)
and chk_varExpr v env del errs =
    match v with
    | SimpleVar {ident; pos} ->
        if Hashtbl.mem !env ident
            then
                Hashtbl.find !env ident
            else
                begin
                errs := ("Variable " ^ ident ^ " undeclared." ^ 
                        (pos_string pos)) :: !errs;
                NONE
                end
    | ArrayVar {arr; idx; pos} -> 
        begin
        if (chk_Expr idx env del errs) <> INT
            then
                errs := ("Non integer index into array." ^ (pos_string pos)) :: !errs;
        let arr_t = if Hashtbl.mem !env arr
                        then
                            Hashtbl.find !env arr
                        else
                            begin
                            errs := ("Variable " ^ arr ^ " undeclared." ^ 
                                    (pos_string pos)) :: !errs;
                            NONE
                            end
        in
        match arr_t with
        | NONE -> NONE
        (* Indexing into an array peels off one layer of ScrawlArrayType *)
        | ScrawlArrayType {array_type; len; _} -> array_type
        | _ ->
            begin
            errs := ("Indexing into non array type." ^ (pos_string pos)) :: !errs;
            NONE
            end
        end

(* For declarations we add the declared variable to the environment.
   TODO 5: care about multiple declarations within the same scope *)
and chk_declExpr d env del errs =
    match d with
    | SimpleDecl {var_type; ident; _} ->
        begin
        add_ident ident var_type env del;
        var_type
        end
    | ArrDecl {arr_type; ident; _} ->
        begin
        add_ident ident arr_type env del;
        arr_type
        end
    | FuncDecl {func_type; ident; params; body; pos} ->
        begin
        add_ident ident func_type env del;
        Stack.push "*" !del; (* Start a new scope *)
        chk_paramList params env del errs; (* Adds each param to the env *)
        let returned = chk_ExprList body env del errs in
        let ret_type = match func_type with
            | ScrawlFuncType {param_types; ret_type} -> ret_type
            | _ -> raise (Invalid_argument "Type of lambda is not function type. (Compiler bug)")
        in
        if (body <> [] && ret_type <> returned)
            then
                errs := ("Returned type " ^ (string_of_type returned) 
                         ^ " does not match declared return type " 
                         ^  (string_of_type ret_type) 
                         ^ " in function definition." ^ (pos_string pos)) :: !errs;
        rewind_env env del;
        func_type
        end

and chk_paramList p env del errs=
    (fun _ -> ()) (List.map (fun y -> chk_Qual y env del errs) p)

and chk_Qual q env del errs =
    match q with
    | QualIdent {ident_type; ident; pos} ->
        add_ident ident ident_type env del

and chk_argList args param_types pos env del errs =
    let arg_types = List.map (fun e -> chk_Expr e env del errs) args in
    let arg_locs = List.map extract_pos args in
    chk_argTypes arg_types param_types arg_locs pos errs

and chk_argTypes arg_types param_types arg_locs pos errs =
    match arg_types, param_types, arg_locs with
    | h1::t1, h2::t2, h3::t3 ->
        begin
        if not (comp_ScrawlType h1 h2) && (h2 <> FLOAT || h1 <> INT)
            then errs := ("Argument does not match parameter type." ^ (pos_string h3)) :: !errs;
        end;
        chk_argTypes t1 t2 t3 pos errs
    | [], [], [] -> ()
    | [], _, _ -> errs := ("Not enough arguments in function call." 
                           ^ (pos_string pos)) :: !errs
    | _, [], _ -> errs := ("Too many arguments in function call."
                           ^ (pos_string pos)) :: !errs
    | _, _, _ -> ()

(* TODO 5: Allow greater diversity in allowed bitwise and logical binop operands.
           (Also for if/for/while conditions)  *)
(* Bitwise ops require integer operands *)
and chk_bitwise_op t1 t2 pos errs =
    if (t1 <> INT)
        then errs := ("Lefthand side of bitwise op does not have type int." 
                      ^ (pos_string pos)) :: !errs;
    if (t2 <> INT)
        then errs := ("Righthand side of bitwise op does not have type int." 
                      ^ (pos_string pos)) :: !errs;

(* Logical ops require boolean operands *)
and chk_logical_op t1 t2 pos errs =
    if (t1 <> BOOL)
        then errs := ("Lefthand side of logical op does not have type bool." 
                      ^ (pos_string pos)) :: !errs;
    if (t2 <> BOOL)
        then errs := ("Righthand side of logical op does not have type bool." 
                      ^ (pos_string pos)) :: !errs;

(* Comparison ops require their operands to have the same type. Array, function,
   and NONE comparisons are disallowed *)
and chk_comp_op t1 t2 pos errs =
    match t1, t2 with
    | INT, INT
    | FLOAT, FLOAT
    | BOOL, BOOL
    | STRING, STRING -> ()
    | NONE, _ -> errs := ("Comparison with type none." ^ (pos_string pos)) :: !errs
    | _, NONE -> errs := ("Comparison with type none." ^ (pos_string pos)) :: !errs
    | ScrawlArrayType _, _ -> errs := ("Comparison with array." 
                                       ^ (pos_string pos)) :: !errs
    | _, ScrawlArrayType _ -> errs := ("Comparison with array." 
                                       ^ (pos_string pos)) :: !errs
    | ScrawlFuncType _, _ -> errs := ("Comparison with function."
                                      ^ (pos_string pos)) :: !errs
    | _, ScrawlFuncType _ -> errs := ("Comparison with function."
                                      ^ (pos_string pos)) :: !errs
    | _, _ -> errs := ("Comparison of unequal types." ^ (pos_string pos)) :: !errs

(* Arithmetic operators require their operands to be ints or floats *)
and chk_arith_op t1 t2 pos errs =
    let success = 
        (if (t1 <> INT) && (t1 <> FLOAT)
            then begin
                 errs := ("Lefthand side of arithmetic op does not have arithmetic type."
                          ^ (pos_string pos)) :: !errs;
                 false
                 end
            else true)
        &&
        (if (t2 <> INT) && (t2 <> FLOAT)
            then begin
                 errs := ("Righthand side of arithmetic op does not have arithmetic type."
                          ^ (pos_string pos)) :: !errs;
                 false
                 end
            else true) in
    if success && ((t1 = FLOAT) || (t2 = FLOAT))
        then FLOAT
        else INT

