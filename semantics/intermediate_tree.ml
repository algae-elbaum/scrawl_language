
(** The intermediate representation of the language, to be a stepping stone
    between the AST and machine code. In practice since we are running out of
    time, this will the the final form of the code, and we'll make an
    an interpreter that can run it *)
type intermediate_tree = INTRM_TREE of expr list

and expr =
    | I_CONST of int
    | F_CONST of float
    | NAME of label
    | TEMP of temp
    | BINOP of binop * expr * expr
    | MEM of expr
    | CALL of expr * expr list
    | ESEQ of stm * expr

and stm =
    | MOVE of expr * expr
    | EXP of expr
    | JUMP of expr * label list
    | CJUMP of relop * expr * expr * label * label
    | SEQ of stm * stm
    | LABEL of label

and binop = 
    | BAND | BOR | BXOR | BLEFT | BRIGHT 
    | LAND | LOR | LXNOR | LXAND | LXNAND 
    | PLUS | MINUS | TIMES | DIV | MOD | POW

and relop = 
    | EQ | LT | GT | LE | GE 

(* As far as I can tell, labels and temps are only meant to be UIDs, and it's
   the responsibility of later parts of the compiler to associate them with the
   actual things in memory to which they refer *)
and label = int

and temp = int

(** For generating unique labels *)
let label_count = ref 0
let new_label () =
    begin
    label_count := !label_count + 1;
    !label_count
    end

(** For generating unique temps *)
let temp_count = ref 0
let new_temp () =
    begin
    temp_count := !temp_count + 1;
    !temp_count
    end

let int_of_bool b =
    if b then 1 else 0

(** Add a new ident to the environments *)
let add_ident ident ident_type loc_env type_env del =
    Hashtbl.add !loc_env ident (new_temp  ());
    Hashtbl.add !type_env ident ident_type;
    Stack.push ident !del

(** Rewind the loc_env type_environment to immediately before it entered the current level of
    scoping. Raises Empty when at global scope. (So don't use it at global scope.
    There should be no reason to do so) *)
let rec rewind_env loc_env type_env del =
    match (Stack.pop !del) with
    | "*" -> () (* A star denotes the beginning of a new scope *)
    | s -> begin
           Hashtbl.remove !loc_env s;
           Hashtbl.remove !type_env s;
           rewind_env loc_env type_env del;
           end

(* Turn a list of stms into a SEQ *)
let rec seq lst =
    let rec lst_to_seq lst =
        match lst with
        | [] -> raise (Invalid_argument "This should be impossible")
        | [stm] -> stm
        | h::t -> SEQ (h, lst_to_seq t)
    in
    match lst with
    | []
    | [_] -> raise (Invalid_argument "Empty or singleton stm list for seq")
    | h::t -> SEQ (h, lst_to_seq t)

let rec intermediate_of_ast (Abstract_syntax.AST tree) =
    (* We'll maintain a mutating loc_env type_environment and pass it around by reference to
     avoid the cost of copying the hash table all the time. A deletion stack
     follows it around and records what must be deleted when leaving an 
     loc_env type_environment. *)
    let loc_env = ref (Hashtbl.create 10) in
    let type_env = ref (Hashtbl.create 10) in
    let del = ref (Stack.create ()) in
    INTRM_TREE (translate_ExprList tree loc_env type_env del);

and translate_ExprList expr_list loc_env type_env del = 
    List.map (fun (e) -> translate_Expr e loc_env type_env del) expr_list

and translate_Expr exp loc_env type_env del =
    match exp with
    | Abstract_syntax.VarExpr var -> translate_varExpr var loc_env type_env del
    | Abstract_syntax.DeclExpr decl -> I_CONST 1
    | Abstract_syntax.AssignExpr {var; value; pos} ->  I_CONST 1
    | Abstract_syntax.LambdaExpr {func_type; params; body; pos} -> 
        begin
        Stack.push "*" !del; (* Start a new scope *)
        rewind_env loc_env type_env del;
        I_CONST 1
        end
    | Abstract_syntax.ReturnExpr x -> I_CONST 1
    | Abstract_syntax.IntLitExpr {value; _} -> I_CONST value
    | Abstract_syntax.FloatLitExpr {value; _} -> F_CONST value
    | Abstract_syntax.StringLitExpr {value; _} -> I_CONST 1 (* Strings to be treated similar to arrays *)
    | Abstract_syntax.BoolLitExpr {value; _} -> I_CONST (int_of_bool value)
    | Abstract_syntax.FuncCallExpr {func; args; pos} -> I_CONST 1
    | Abstract_syntax.BinOpExpr {op; argl; argr; pos} -> 
        begin
        match op with
        |Abstract_syntax.BAND -> I_CONST 1
        |Abstract_syntax.BOR -> I_CONST 1
        |Abstract_syntax.BXOR -> I_CONST 1
        |Abstract_syntax.BLEFT -> I_CONST 1
        |Abstract_syntax.BRIGHT -> I_CONST 1
        |Abstract_syntax.LAND -> I_CONST 1
        |Abstract_syntax.LOR -> I_CONST 1
        |Abstract_syntax.LXNOR -> I_CONST 1
        |Abstract_syntax.LXAND -> I_CONST 1
        |Abstract_syntax.LXNAND -> I_CONST 1
        |Abstract_syntax.EQ -> I_CONST 1
        |Abstract_syntax.LESS -> I_CONST 1
        |Abstract_syntax.GREATER -> I_CONST 1
        |Abstract_syntax.PLUS -> I_CONST 1
        |Abstract_syntax.MINUS -> I_CONST 1
        |Abstract_syntax.TIMES -> I_CONST 1
        |Abstract_syntax.DIV -> I_CONST 1
        |Abstract_syntax.MOD -> I_CONST 1
        |Abstract_syntax.POW -> I_CONST 1
        end

    | Abstract_syntax.UnOpExpr {op; arg; pos} -> I_CONST 1

    | Abstract_syntax.IfExpr {cond; body;  else_expr; pos} ->  I_CONST 1
    | Abstract_syntax.ForExpr _  -> 
        raise (Invalid_argument "Translator got AST with for loop (compiler bug)")
    | Abstract_syntax.WhileExpr {cond; body; preface; pos} -> I_CONST 1
    | Abstract_syntax.NoOp -> I_CONST 1

and translate_varExpr var loc_env type_env del =
    match var with
    | Abstract_syntax.SimpleVar {ident; _} -> Hashtbl.find !loc_env ident (* Return the associated temp *)
    | Abstract_syntax.ArrayVar _ -> translate_array var loc_env type_env del

and translate_array arr loc_env type_env del =
    let rec translate_indexing arr_loc arr_type a =
        match arr_type with
        | Abstract_syntax.ScrawlArrayType {array_type; len; _} ->
            let (arr, idx) = match a with
                             | Abstract_syntax.ArrayVar {arr; idx; _} -> (arr, idx)
                             | _ -> raise (Invalid_argument "This should be impossible")
            in
            let trans_idx = translate_Expr idx loc_env type_env del in
            (* Prevent array out of bounds access: *)
            let in_bound = new_label () in
            let maybe_in_bound = new_label () in
            let out_of_bound = new_label () in
            ESEQ ((seq [CJUMP (LT, trans_idx, I_CONST len, maybe_in_bound, out_of_bound);
                        LABEL maybe_in_bound;
                        CJUMP (GE, trans_idx, I_CONST 0, in_bound, out_of_bound);
                        LABEL out_of_bound;
                        (* TODO have some mechanism for crashing the program with an error message *)
                        LABEL in_bound;]),
                 (* Return the address of the desired element *)
                  BINOP (PLUS, translate_indexing arr_loc array_type arr, trans_idx)) 
        | _ -> arr_loc
    in
    let arr_ident = Abstract_syntax.ident_of_var arr in
    let arr_type = Hashtbl.find !type_env arr_ident in
    translate_indexing (Hashtbl.find !loc_env arr_ident) arr_type arr

