open Intermediate_tree

type num = INT of int | FLOAT of float

let array_vars = ref (Array.make 10 0) 
let stack_pointer = ref 0 
(* let vars : (Intermediate_tree.temp, num) Hashtbl.t= ref (Hashtbl.create 10)  *)
let glblJmp = ref 0 

let rec interp_tree tree =
    match tree with
    | INTRM_TREE x -> 
            let ans = interp_expr x 0 tree in
            ans
    | _ -> raise (Invalid_argument "Should never happen")

(* and interp_exprlist lst jmp whole=
    match lst with
    | (xs::xss) -> 
        begin
        interp_expr xs jmp whole;
        if !glblJmp = jmp
            then interp_exprlist xss jmp whole
        end
    | [] -> 0 (*Not sure about this*) *)
and interp_expr xpr jmp whole= (*The rest is only used for labels*)
    match xpr with
    | I_CONST x -> INT x
    | F_CONST x -> FLOAT x
    (* Thing to jump to.It expects to get a number that corresponds to a label *)
    (* | NAME x -> x *)
    (* Temp a = Stack[Hastabl.find x] *)
 (*    | TEMP x -> 
        let pointer = Hashtbl.find !vars x in
        Hashtbl.find !array_vars pointer *)
    (* The only way to write a var is with move, so we're going to be silly when we use move*)
    | BINOP (op, x1, x2) -> (interp_binop op (interp_expr_val x1) (interp_expr_val x2))
    (* | ALLOC_MEM (temp, i) -> begin
        (* Everything is an array. even singletons *)
        Hashtbl.add !vars temp !stack_pointer;
        (* Add to the stack pointer *)
        stack_pointer = (!stack_pointer) + i
    end *)
    (* Mem x = Hastabl.find x *)
    (* | MEM x ->
        let pointer = Hashtbl.find !vars x *)
    (* | CALL (f, xprLst) -> begin
        interp_exprlist xprLst;
        match f with
        | TEMP x -> interp_expr x
        | LABEL x -> interp_expr x
        | _ -> raise (Invalid_argument "Should never happen. Functions should be places")
    end *)
    | ESEQ (s, x) -> 
        begin
            let temp = interp_stm s jmp whole in
            if (!glblJmp = jmp)
                then (interp_expr x jmp whole)
                else temp
        end
    | _ -> raise (Invalid_argument "Should never happen")
and interp_expr_val xpr =
    match xpr with
    | I_CONST x -> INT x
    | F_CONST x -> FLOAT x
    (* | TEMP x ->
    | CALL x -> 
    | MEM x ->
    | MEM_TEMP x -> *)
    | _ -> raise (Invalid_argument "Should never happen")
(* Takes a binop and returns back a num *)
and interp_binop op x1 x2 =
    match op, x1, x2 with
    | BAND, INT y1, INT y2 -> INT(y1 land y2)
    | BAND, _, _ -> raise (Invalid_argument "Needs booleans")

    | BOR, INT y1, INT y2 -> INT(y1 lor y2)
    | BOR, _, _ -> raise (Invalid_argument "Needs booleans")

    | BXOR, INT y1, INT y2 -> INT(y1 lxor y2)
    | BXOR, _, _ -> raise (Invalid_argument "Needs booleans")

    | BRIGHT, INT y1, INT y2 -> INT(y1 lsl y2)
    | BRIGHT, _, _ -> raise (Invalid_argument "Needs booleans")

    | BLEFT, INT y1, INT y2 -> INT(y1 lsr y2)
    | BLEFT, _, _ -> raise (Invalid_argument "Needs booleans")

    (* Logical stuff takes Bools, which are 0s or 1s. *)
    | LAND, INT y1, INT y2 -> INT(if (y1 + y2 > 0) then 1 else 0)
    | LAND, _, _ -> raise (Invalid_argument "Needs booleans")

    | LOR, INT y1, INT y2 -> INT(if (y1 + y2 > 0) then 1 else 0)
    | LOR, _, _ -> raise (Invalid_argument "Needs booleans")

    | LXNOR, _, _-> raise (Invalid_argument "This function isn't implemented. How did this even happen?")
    | LXAND, _, _-> raise (Invalid_argument "This function isn't implemented. How did this even happen?")
    | LXNAND, _, _-> raise (Invalid_argument "This function isn't implemented. How did this even happen?")

    | PLUS, INT y1, FLOAT y2 -> FLOAT(float_of_int(y1) +. y2)
    | PLUS, FLOAT y1, FLOAT y2 -> FLOAT(y1 +. y2)
    | PLUS, FLOAT y1, INT y2 -> FLOAT(y1 +. float_of_int(y2))
    | PLUS, INT y1, INT y2 -> INT(y1 + y2)

    | MINUS, INT y1, FLOAT y2 -> FLOAT(float_of_int(y1) -. y2)
    | MINUS, FLOAT y1, FLOAT y2 -> FLOAT(y1 -. y2)
    | MINUS, FLOAT y1, INT y2 -> FLOAT(y1 -. float_of_int(y2))
    | MINUS, INT y1, INT y2 -> INT(y1 - y2)

    | TIMES, INT y1, FLOAT y2 -> FLOAT(float_of_int(y1) *. y2)
    | TIMES, FLOAT y1, FLOAT y2 -> FLOAT(y1 *. y2)
    | TIMES, FLOAT y1, INT y2 -> FLOAT(y1 *. float_of_int(y2))
    | TIMES, INT y1, INT y2 -> INT(y1 * y2)

    | DIV, INT y1, FLOAT y2 -> FLOAT(float_of_int(y1) /. y2)
    | DIV, FLOAT y1, FLOAT y2 -> FLOAT(y1 /. y2)
    | DIV, FLOAT y1, INT y2 -> FLOAT(y1 /. float_of_int(y2))
    | DIV, INT y1, INT y2 -> INT(y1 / y2)
    
    | MOD, INT y1, INT y2 -> INT(y1 / y2)
    | MOD, _, _ -> raise (Invalid_argument "Needs only ints")
    
    | POW, INT y1, FLOAT y2 -> FLOAT(float_of_int(y1) ** y2)
    | POW, FLOAT y1, FLOAT y2 -> FLOAT(y1 ** y2)
    | POW, FLOAT y1, INT y2 -> FLOAT(y1 ** float_of_int(y2))
    | POW, INT y1, INT y2 -> INT(int_of_float(float_of_int(y1) ** float_of_int(y2)))

    | _ -> raise (Invalid_argument "Should never happen")
and interp_stm statement jmp whole=
    match statement with
(*    | MOVE (t, x) -> 
        let pointer = Hashtbl.find !vars t in
        let value = (interp_expr x) in 
        Hashtbl.add !array_vars pointer value *)
(*     | EXP (x) -> interp_expr x *)
    | SEQ (s1, s2) -> begin
        interp_stm s1 jmp whole;
        interp_stm s2 jmp whole
    end
    (* Unconditional jump to a label *)
    | JUMP l -> let rest = look_for_label l whole whole in(* Hashtbl.find !labels l in *)
        begin
            glblJmp := !glblJmp + 1;
            interp_expr rest (jmp+1) whole
        end
    | CJUMP (r, x1, x2, l1, l2) -> let x1' = interp_expr x1 jmp whole in
        let x2' = interp_expr x2 jmp whole in
        let r' = interp_relop r x1' x2' in
        if (r' = (INT 1))
            then
                let rest = look_for_label l1 whole whole in
                begin
                    glblJmp := !glblJmp + 1;
                    interp_expr rest (jmp+1) whole
                end
            else
                let rest = look_for_label l2 whole whole in
                begin
                    glblJmp := !glblJmp + 1;
                    interp_expr rest (jmp+1) whole
                end
    (* Sets up a label. Shouldn't actually do anything
    because jumps will look for the labels. *)
    (* | LABEL (l) -> 0 *)
    | _ -> raise (Invalid_argument "Should never happen")
and interp_relop op x1 x2=
    match op , x1, x2 with
    | EQ, INT y1, INT y2 -> if y1 = y2 then INT 1 else INT 0
    | EQ, FLOAT y1, FLOAT y2 -> if y1 = y2 then INT 1 else INT 0

    | LT, INT y1, INT y2 -> if y1 < y2 then INT 1 else INT 0
    | LT, FLOAT y1, FLOAT y2 -> if y1 < y2 then INT 1 else INT 0 
    
    | GT, INT y1, INT y2 -> if y1 > y2 then INT 1 else INT 0
    | GT, FLOAT y1, FLOAT y2 -> if y1 > y2 then INT 1 else INT 0 
    
    | LE, INT y1, INT y2 -> if y1 <= y2 then INT 1 else INT 0
    | LE, FLOAT y1, FLOAT y2 -> if y1 <= y2 then INT 1 else INT 0 
    
    | GE, INT y1, INT y2 -> if y1 >= y2 then INT 1 else INT 0
    | GE, FLOAT y1, FLOAT y2 -> if y1 >= y2 then INT 1 else INT 0
    
    | _, INT y1, FLOAT y2 -> raise (Invalid_argument "Ints and floats cannot be compared")
    | _, FLOAT y1, INT y2 -> raise (Invalid_argument "Ints and floats cannot be compared")

and look_for_label val1 xpr whole = 
    match xpr with
    |  INTRM_TREE  x-> 
        match x with 
        | ESEQ (y1, x2) -> if (look_for_label_stm val1 y1 whole) 
                            then x 
                            else (look_for_label val1 (INTRM_TREE x2) whole)
        | _ -> raise (Invalid_argument "Should never happen") (*Not any labels bc labels are STMs so can't happen*)

and look_for_label_stm val1 stm whole=
    match stm with
    | LABEL z1 -> if (val1 = z1) then true else false
    (* What we expect *)
    | SEQ (z1, z2) -> if (look_for_label_stm val1 z1 whole) then true else (look_for_label_stm val1 z2 whole)
    (* Possibly more than one stm in a row *)
    | EXP (x) -> (look_for_label_expr val1 x whole)
    | _ -> false
and look_for_label_expr val1 expr whole=
    match expr with
    | ESEQ (y1, x2) -> if (look_for_label_stm val1 y1 whole)
                            then true
                        else (look_for_label_expr val1 x2 whole)
    | _ -> false