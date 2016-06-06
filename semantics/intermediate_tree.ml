
(** The intermediate representation of the language, to be a stepping stone
    between the AST and machine code. In practice since we are running out of
    time, this will the the final form of the code, and we'll make an
    an interpreter that can run it *)
type intermediate_tree = INTRM_TREE of expr list

and expr =
    | I_CONST of int
    | F_CONST of float
    (* See below for what labels and temps are *)
    | NAME of label
    | TEMP of temp
    | BINOP of binop * expr * expr
    (* temps map to memory locations (At least for now, at least for the interpreter, we're
       not doing anything resembling registers). MEM of a temp gives the memory location. *)
    | MEM of temp
    (*  MEM_TEMP dereferences MEMs. The expr will never not be a a MEM. If t is a temp then
        (MEM_TEMP (MEM t)) should act the same as (TEMP t). The idea is that 
        (MEM_TEMP (PLUS (MEM t) e)) should be able to act as a temp for the memory slot e
        slots down from (MEM t). This is for arrays and strings. *) 
    | MEM_TEMP of expr
    (*  The int in ALLOC_MEM is the number of words the temp will need if written to memory.
        I'm having trouble understanding how the book wants to do this, since the book doesn't
        have an ALLOC_MEM. The book seems to maintain in parallel a frame data structure. This
        here feels simpler and clearer, especially since at this point we're aiming for having
        an interpreter for this tree, rather than compiling all the way to x86. The idea for
        this is that the interpreter would maintain an environment similar to the ones maintained
        here. Upon ALLOC_MEM it would make some storage somehow, and the environment would map
        temps to their storage. (Compiling to x86 would do essentially the same thing and probably
        have an easier time of it) *)
    | ALLOC_MEM of temp * int
    (* (UNALLOC_MEM n) signals that the last n words of memory are now out of scope and should
       be unallocated *)
    | UNALLOC_MEM of int
    (* CALL of func * args. I think the func should only ever be a NAME or a TEMP, but TODO make sure *)
    | CALL of expr * expr list
    | ESEQ of stm * expr

and stm =
    (* The left of MOVE will always either be a temp or a MEM_VAL *)
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
(* A label identifies a location in the code that can be jumped to *)
and label = int

(* A temp identifies a variable. *)
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
    let n_temp = new_temp () in
    Hashtbl.add !loc_env ident n_temp;
    Hashtbl.add !type_env ident ident_type;
    Stack.push ident !del;
    n_temp

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
    | Abstract_syntax.DeclExpr decl -> translate_declExpr decl loc_env type_env del
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
    | Abstract_syntax.SimpleVar {ident; _} -> TEMP (Hashtbl.find !loc_env ident) (* Return the associated temp *)
    | Abstract_syntax.ArrayVar {arr; idx; _} ->
        let arr_type = Hashtbl.find !type_env arr in
        match arr_type with
        | Abstract_syntax.ScrawlArrayType {array_type; len; _} ->
            let trans_idx = translate_Expr idx loc_env type_env del in
            let arr_temp = Hashtbl.find !loc_env arr in
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
                  (* Return the (MEM_VAL (MEM _)) of the desired element *)
                  (* Array indexing works by adding the index to the value in the temp that
                     identifies the start of the array (or in the case of multidim indexing,
                     the value that a temp would have if one were assigned to the relevant
                     sub-array) *)
                  (MEM_TEMP (BINOP (PLUS, (MEM arr_temp), trans_idx))))
        | _ -> raise (Invalid_argument "This should be impossible")

and translate_declExpr decl loc_env type_env del =
    match decl with
    (* For simple decls we just make a new temp for the variable  *)
    | Abstract_syntax.SimpleDecl {var_type; ident; pos} ->
        let tmp = add_ident ident var_type loc_env type_env del in
        ALLOC_MEM (tmp, 1)
    | Abstract_syntax.ArrDecl {arr_type; ident; pos} ->
        (* !!!Arrays are only single dimensional!!! *)
        begin
            let tmp = add_ident ident arr_type loc_env type_env del in
            match arr_type with
                | Abstract_syntax.ScrawlArrayType {array_type; len; _} -> ALLOC_MEM (tmp, len)
                | _ -> raise (Invalid_argument "This should be impossible")
        end
    | Abstract_syntax.FuncDecl {func_type; ident; params; body; pos} -> I_CONST 1
       (* let f_start = new_label () in
        ESEQ
*)

and translate_block block loc_env type_env del =
    I_CONST 1
