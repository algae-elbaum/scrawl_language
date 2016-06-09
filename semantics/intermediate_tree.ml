

(** The intermediate representation of the language, to be a stepping stone
    between the AST and machine code. In practice since we are running out of
    time, this will the the final form of the code, and we'll make an
    an interpreter that can run it *)
type intermediate_tree = INTRM_TREE of expr

and expr =
    | I_CONST of int
    | F_CONST of float
    (* See below for what labels and temps are *)
    | NAME of label
    (*  TEMP acts as a variable. Each temp t should map to a memory location (or simulated
        memory location in the case of the interpreter), and then (TEMP t) represents the
        contents of that location. *)
    | TEMP of temp
    | BINOP of binop * expr * expr
    (* temps map to memory locations (At least for now, at least for the interpreter, we're
       not doing anything resembling registers). MEM of a temp gives the memory location. *)
    | MEM of temp
    (*  MEM_TEMP dereferences MEMs. If t is a temp then (MEM_TEMP (MEM t)) should act the
        same as (TEMP t). The idea is that (MEM_TEMP (PLUS (MEM t) e)) should be able to
        act exactly like a temp for the memory slot e slots down from (MEM t). This should
        work even with multiple layers, ie (MEM_TEMP (MEM (MEM_TEMP (MEM t)))) should still
        act just the same as (TEMP t). This is for arrays to work nicely. *) 
    | MEM_TEMP of expr
    | ESEQ of stm * expr

and stm =
    (* The left of MOVE will always either be a TEMP, a MEM_TEMP, or a NAME. When it is a NAME
       the right will also be a NAME, and the left NAME should be set to act as an alias for the
       right NAME *)
    | MOVE of expr * expr
    (*  The right hand side of COPY will always evaluate to an integer value, where the value is that
        of a temp that has already been defined. The result of a COPY should be that the left hand temp
        will act in every way like the right hand temp. I expect that this should be implemented by
        just setting the temp on the left to map to the same thing that the temp on the right does. *)
    | COPY of temp * expr
    | EXP of expr
    | JUMP of label
    | CJUMP of relop * expr * expr * label * label
    (* CALL of func * ret_loc * args. The func will only ever be a NAME. A CALL must make the ret_loc
       label act precisely as if it is the label provided to the CALL *)
    | CALL of expr * label * expr list
    | SEQ of stm * stm
    (* A label declared a label that may be JUMPed or CJUMPed to later *)
    | LABEL of label
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
    | PRINT of string


and binop = 
    | BAND | BOR | BXOR | BLEFT | BRIGHT 
    | LAND | LOR | LXNOR | LXAND | LXNAND 
    | PLUS | MINUS | TIMES | DIV | MOD | POW

and relop = 
    | EQ | LT | GT | LE | GE 

(* A label identifies a location in the code that can be jumped to *)
and label = int

(* A temp identifies a variable. *)
and temp = int

let rec prettyPrint_Stm s =
    match s with
    | MOVE (x1,x2) -> "MOVE " ^ (prettyPrint_Expr x1) ^ ", " ^ (prettyPrint_Expr x2)
    | COPY (t, x) -> "COPY " ^ (string_of_int t) ^ ", " ^ (prettyPrint_Expr x)
    | EXP x -> "\n" ^ (prettyPrint_Expr x)
    | JUMP l-> "JUMP " ^ (string_of_int l)
    | CJUMP (relop,x1,x2,l1,l2)-> "CJUMP {(" ^ (prettyPrint_Expr x1) ^ "-> " ^ (string_of_int l1) ^ ") (" ^ (prettyPrint_Expr x2) ^ "-> " ^ (string_of_int l2) ^ ")}"
    (* I don't think the label in call needs to go i nthe pretty print *)
    | CALL (x,l,args) -> "CALL {" ^ (prettyPrint_Expr x) ^ "on (" ^ (prettyPrint_ParamList args) ^ ")}"
    | SEQ (s1, s2) -> (prettyPrint_Stm s1) ^ "\n" ^ (prettyPrint_Stm s2)
    | LABEL l -> "LABEL " ^ (string_of_int l)
    | ALLOC_MEM (t,i) -> "ALLOC_MEM " ^ (string_of_int i) ^ " slots to " ^ (string_of_int t)
    | UNALLOC_MEM i -> "UNALLOC_MEM " ^ (string_of_int i)
    | PRINT str -> "PRINT " ^ str

and prettyPrint_ParamList args =
    match args with
    | (xs::xss) -> (prettyPrint_Expr xs) ^ ", " ^ (prettyPrint_ParamList xss)
    | [] -> ""

and prettyPrint_RelOp op =
    match op with
    | EQ -> "="
    | LT -> "<"
    | GT -> ">"
    | LE -> "<="
    | GE -> ">="

and prettyPrint_BinOp op =
    match op with
    | BAND -> "BAND"
    | BOR -> "BOR"
    | BXOR ->"BXOR"
    | BLEFT -> "BLEFT"
    | BRIGHT -> "BRIGHT"
    | LAND -> "LAND"
    | LOR -> "LOR"
    | LXNOR ->"LXNOR"
    | LXAND ->"LXAND"
    | LXNAND ->"LXNAND"
    | PLUS ->"PLUS"
    | MINUS ->"MINUS"
    | TIMES ->"TIMES"
    | DIV ->"DIV"
    | MOD ->"MOD"
    | POW ->"POW"

and prettyPrint_Expr xpr =
    match xpr with
    | I_CONST x -> string_of_int x
    | F_CONST x -> string_of_float x
    | NAME l -> "NAME" ^ (string_of_int l)
    | TEMP t -> "TEMP" ^ (string_of_int t)
    | BINOP (op, x1, x2) -> "(" ^(prettyPrint_Expr x1) ^" "^(prettyPrint_BinOp op)^" " ^ (prettyPrint_Expr x1) ^ ")"
    | MEM t -> "TEMP" ^ (string_of_int t)
    | MEM_TEMP x -> "MEM_TEMP" ^ (prettyPrint_Expr x)
    | ESEQ (s, x) -> (prettyPrint_Stm s) ^ "\n" ^ (prettyPrint_Expr x)

and prettyPrint_Tree tree = 
    match tree with
    | INTRM_TREE x -> prettyPrint_Expr x

(** For generating unique labels *)
let label_count = ref 0
let new_label () =
    begin
    label_count := !label_count + 1;
    !label_count
    end

(* Predefined temps: *)

(* At the time of a function call this must be set by the interpreter/future part
   of the compiler to an array of values. Those values will be taken as the TEMPs
   of the args to the function. This is to make passing arrays as arguments more feasible *)
let arg_temp = 0 

(* At the end of a function call (TEMP ret_val) will be set to the return value of the function. *)
let ret_val = 1

(* At the time of a function call, (NAME ret_loc) must be made to act as the proper return
   location of the functions. Functions will return with the stm (JUMP (NAME ret_loc)) *)
let ret_loc = 1

(** For generating unique temps *)
let temp_count = ref 2
let new_temp () =
    begin
    temp_count := !temp_count + 1;
    !temp_count
    end

let int_of_bool b =
    if b then 1 else 0

(** Add a new ident to the environments *)
let add_temp_ident ident ident_type temp_env type_env del =
    let n_temp = new_temp () in
    Hashtbl.add !temp_env ident n_temp;
    Hashtbl.add !type_env ident ident_type;
    Stack.push ident !del;
    n_temp

let add_label_ident ident lab_env =
    let n_lab = new_label () in
    Hashtbl.add !lab_env ident n_lab;
    n_lab

(* Turn a list of stms into a SEQ *)
let rec seq lst =
    let rec lst_to_seq lst =
        match lst with
        | [] -> raise (Invalid_argument "This should be impossible")
        | [stm] -> stm
        | h::t -> SEQ (h, lst_to_seq t)
    in
    match lst with
    | [] -> raise (Invalid_argument "Empty stm list for seq")
    | [e] -> e
    | h::t -> SEQ (h, lst_to_seq t)

let rec intermediate_of_ast (Abstract_syntax.AST tree) =
    (* We'll maintain mutating temp_env, lab_env, and type_env environments
       and pass them around by reference. A deletion stack follows them around
       and records what must be deleted when leaving a scope. *)
    let temp_env = ref (Hashtbl.create 10) in
    let lab_env = ref (Hashtbl.create 10) in
    let type_env = ref (Hashtbl.create 10) in
    let del = ref (Stack.create ()) in
    INTRM_TREE (ESEQ ((translate_block tree [] false temp_env lab_env type_env del), I_CONST 1))

and translate_Expr_expr exp temp_env lab_env type_env del =
    match exp with
    | Abstract_syntax.VarExpr var -> translate_varExpr var temp_env lab_env type_env del
    | Abstract_syntax.AssignExpr {var; value; _} ->
        let l = translate_varExpr var temp_env lab_env type_env del in
        let r = translate_Expr_expr value temp_env lab_env type_env del in
        ESEQ (MOVE (l, r),
              r)
    | Abstract_syntax.LambdaExpr {func_type; params; body; _} ->
        let f_start = new_label () in
        let t_body = translate_block body params true temp_env lab_env type_env del in
        ESEQ (seq [LABEL f_start;
                   t_body], 
              NAME f_start)
    | Abstract_syntax.IntLitExpr {value; _} -> I_CONST value
    | Abstract_syntax.FloatLitExpr {value; _} -> F_CONST value
    (* Strings are not supported yet and will not be supported by the end of term *)
    | Abstract_syntax.StringLitExpr {value; _} -> I_CONST 1 (* Strings to be treated similar to arrays *)
    | Abstract_syntax.BoolLitExpr {value; _} -> I_CONST (int_of_bool value)
    | Abstract_syntax.FuncCallExpr {func; args; _} ->
        let f = NAME (Hashtbl.find !lab_env func) in
        let args = List.map (fun e -> translate_Expr_expr e temp_env lab_env type_env del) args in
        let l = new_label () in
        ESEQ (seq [CALL (f, l, args);
                   LABEL l],
              TEMP ret_val)
    | Abstract_syntax.BinOpExpr {op; argl; argr; _} -> 
        begin
            let (t_argl, t_argr) = (translate_Expr_expr argl temp_env lab_env type_env del,
                                    translate_Expr_expr argr temp_env lab_env type_env del) in
            match op with
            |Abstract_syntax.BAND -> BINOP(BAND, t_argr, t_argl)
            |Abstract_syntax.BOR -> BINOP (BOR, t_argr, t_argl)
            |Abstract_syntax.BXOR -> BINOP (BXOR, t_argr, t_argl)
            |Abstract_syntax.BLEFT -> BINOP (BLEFT, t_argr, t_argl)
            |Abstract_syntax.BRIGHT -> BINOP (BRIGHT, t_argr, t_argl)
            |Abstract_syntax.LAND -> BINOP (LAND, t_argr, t_argl)
            |Abstract_syntax.LOR -> BINOP (LOR, t_argr, t_argl)
            |Abstract_syntax.LXNOR -> BINOP (LXNOR, t_argr, t_argl)
            |Abstract_syntax.LXAND -> BINOP (LXAND, t_argr, t_argl)
            |Abstract_syntax.LXNAND -> BINOP (LXNAND, t_argr, t_argl)
            |Abstract_syntax.PLUS -> BINOP (PLUS, t_argr, t_argl)
            |Abstract_syntax.MINUS -> BINOP (MINUS, t_argr, t_argl)
            |Abstract_syntax.TIMES -> BINOP (TIMES, t_argr, t_argl)
            |Abstract_syntax.DIV -> BINOP (DIV, t_argr, t_argl)
            |Abstract_syntax.MOD -> BINOP (MOD, t_argr, t_argl)
            |Abstract_syntax.POW -> BINOP (POW, t_argr, t_argl)

            |Abstract_syntax.EQ -> rel_expr EQ t_argl t_argr
            |Abstract_syntax.LESS -> rel_expr LT t_argl t_argr
            |Abstract_syntax.GREATER -> rel_expr GT t_argl t_argr
        end
    | Abstract_syntax.UnOpExpr {op; arg; _} ->
        begin
            let t_arg = translate_Expr_expr arg temp_env lab_env type_env del in
            match op with
            | Abstract_syntax.BNOT -> BINOP (BXOR, I_CONST 1, t_arg)
            (* LNOT works since translation turns binary values into 0 or 1 *)
            | Abstract_syntax.LNOT -> rel_expr EQ (I_CONST 0) t_arg
            | Abstract_syntax.UMINUS -> BINOP (MINUS, I_CONST 0, t_arg)
        end
    | Abstract_syntax.NoOp -> I_CONST 1
    | _ -> raise (Invalid_argument "I don't think this should ever happen, but I'm not 100%")

and translate_Expr_stm exp temp_env lab_env type_env del =
    match exp with
    | Abstract_syntax.DeclExpr decl -> translate_declExpr decl temp_env lab_env type_env del
    | Abstract_syntax.ReturnExpr x ->
        let ret = translate_Expr_expr x temp_env lab_env type_env del in
        seq [MOVE (TEMP ret_val, ret);
             JUMP ret_loc]
    | Abstract_syntax.IfExpr {cond; body; else_expr; _} ->
        let t_cond = translate_Expr_expr cond temp_env lab_env type_env del in
        let t_body = translate_block body [] false temp_env lab_env type_env del in
        let t_else = translate_block else_expr [] false temp_env lab_env type_env del in
        let t = new_label () in
        let f = new_label () in
        let e = new_label () in
        seq [CJUMP (EQ, I_CONST 1, t_cond, t, f);
             LABEL t;
             t_body;
             JUMP e;
             LABEL f;
             t_else;
             LABEL e;]
    | Abstract_syntax.ForExpr _  -> 
        raise (Invalid_argument "Translator got AST with for loop (compiler bug)")
    | Abstract_syntax.WhileExpr {cond; body; preface; _} ->
        let t_preface = translate_Expr_expr preface temp_env lab_env type_env del in
        let t_cond = translate_Expr_expr cond temp_env lab_env type_env del in
        let t_body = translate_block body [] false temp_env lab_env type_env del in
        let c = new_label () in
        let e = new_label () in
        seq [EXP t_preface;
             CJUMP (EQ, I_CONST 1, t_cond, c, e);
             LABEL c;
             t_body;
             CJUMP (EQ, I_CONST 1, t_cond, c, e);
             LABEL e;]
    | Abstract_syntax.PrintExpr {str; _} -> PRINT str
    | _ -> EXP (translate_Expr_expr exp temp_env lab_env type_env del)

and translate_varExpr var temp_env lab_env type_env del =
    match var with
    | Abstract_syntax.SimpleVar {ident; _} -> 
        if Hashtbl.mem !temp_env ident
            then TEMP (Hashtbl.find !temp_env ident) (* Return the associated TEMP *)
            else NAME (Hashtbl.find !lab_env ident) (* Return the associated NAME *)
    | Abstract_syntax.ArrayVar {arr; idx; _} ->
        let arr_type = Hashtbl.find !type_env arr in
        match arr_type with
        | Abstract_syntax.ScrawlArrayType {array_type; len; _} ->
            let trans_idx = translate_Expr_expr idx temp_env lab_env type_env del in
            let arr_temp = Hashtbl.find !temp_env arr in
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

and translate_declExpr decl temp_env lab_env type_env del =
    match decl with
    (* For simple decls we just make a new temp for the variable  *)
    | Abstract_syntax.SimpleDecl {var_type; ident; pos} ->
        let tmp = add_temp_ident ident var_type temp_env type_env del in
        ALLOC_MEM (tmp, 1)
    | Abstract_syntax.ArrDecl {arr_type; ident; pos} ->
        (* !!!Arrays are only single dimensional!!! *)
        begin
            let tmp = add_temp_ident ident arr_type temp_env type_env del in
            match arr_type with
                | Abstract_syntax.ScrawlArrayType {array_type; len; _} -> ALLOC_MEM (tmp, len)
                | _ -> raise (Invalid_argument "This should be impossible")
        end
    | Abstract_syntax.FuncDecl {func_type; ident; params; body; pos} ->
        let f_start = add_label_ident ident lab_env in
        let t_body = translate_block body params true temp_env lab_env type_env del in
        seq [LABEL f_start;
             t_body]

and rel_expr op argl argr =
    let t = new_label () in
    let f = new_label () in
    let e = new_label () in
    let res = new_temp () in
    ESEQ ((seq [ALLOC_MEM (res, 1);
                CJUMP (op, argl, argr, t, f);
                LABEL t;
                MOVE (TEMP res, I_CONST 1);
                JUMP e;
                LABEL f;
                MOVE (TEMP res, I_CONST 0);
                LABEL e;]),
          (TEMP res))

and translate_block block args is_func temp_env lab_env type_env del =
    begin
    (* Start a new scope *)
    Stack.push "*" !del; 
    (* fetch each arg from the arg register before executing *)
    let fetch_args = 
        (List.mapi (fun i (Abstract_syntax.QualIdent {ident_type; ident; _}) ->
                                let n_temp = add_temp_ident ident ident_type temp_env type_env del in
                                COPY (n_temp, (MEM_TEMP (BINOP (PLUS, (MEM arg_temp), I_CONST i)))))
                   args)
    in
    let block_stms = (List.map (fun s -> translate_Expr_stm s temp_env lab_env type_env del) block)
    in
    rewind_env temp_env type_env del; (* TODO call UNALLOC_MEM *)
    if is_func 
        then seq (fetch_args @ block_stms @ [(JUMP ret_loc)])
        else seq (fetch_args @ block_stms)
    end

(** Rewind the temp_env and type_env environments to immediately before it 
    entered the current level of scoping. Raises Empty when at global scope.
    (So don't use it at global scope. There should be no reason to do so) *)
and rewind_env temp_env type_env del =
    match (Stack.pop !del) with
    | "*" -> () (* A star denotes the beginning of a new scope *)
    | s -> begin
           Hashtbl.remove !temp_env s;
           Hashtbl.remove !type_env s;
           rewind_env temp_env type_env del;
           end


