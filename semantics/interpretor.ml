open Intermediate_tree

type num = INT of int | FLOAT of float


(* GlobalJump tells us how many jumps we have preformed.
It is necesary to tell when we need to halt execution of the next expr 
because we already jumped.

Vars tells us what Temps have mapped to what data.
The stack pointer tells us what the most recent data on array_vars is.
Array_vars  is where we store the data that is in arrays. Where that 
data is is stored by temps in vars.*)
let array_vars = ref (Array.make 10 (INT 0))
let func_args = ref (Array.make 10 (INT 0)) (* Not sure what happens when more then 10 args*)
let arg_number = ref 0
let stack_pointer = ref 0 
let vars : (int, num) Hashtbl.t ref = ref (Hashtbl.create 10) 
let glblJmp = ref 0 

let rec interp_tree tree =
    match tree with
    | INTRM_TREE x -> 
            let ans = interp_expr x 0 tree in
            ans
    | _ -> raise (Invalid_argument "Should never happen")

and interp_expr xpr jmp whole= (*The rest is only used for labels*)
    match xpr with
    | I_CONST x -> interp_expr_val xpr
    | F_CONST x -> interp_expr_val xpr
    (* Thing to jump to.It expects to get a number that corresponds to a label *)
    (* | NAME x -> interp_expr_val xpr *)
    (* Temp a = Stack[Hastabl.find x] *)
    | TEMP x -> interp_expr_val xpr
    | MEM_TEMP x -> interp_expr_val xpr
    (* The only way to write a var is with move, so we're going to be silly when we use move*)
    | BINOP (op, x1, x2) -> (interp_binop op (interp_expr_val x1) (interp_expr_val x2))
    (* Mem x = Hastabl.find x *)
    | MEM x -> interp_expr_val xpr
        
    
    | ESEQ (s, x) -> 
        begin
            let temp = interp_stm s jmp whole in
            if (!glblJmp = jmp)
                then (interp_expr x jmp whole)
                else temp
        end
    | _ -> raise (Invalid_argument "Should never happen")
(* This function evaluates the exprs that can be evaluated
and returns the num that should be returned. Should only be called
when we know that the type is going to be in this list. *)
and interp_expr_val xpr =
    match xpr with
    | I_CONST x -> INT x
    | F_CONST x -> FLOAT x
    | TEMP x -> Array.get !array_vars x
    (* | CALL x ->  *)
    | MEM x -> let pointer = Hashtbl.find !vars x in pointer
    | MEM_TEMP x -> let pointer = interp_expr_val x in
        begin
            match pointer with
            | INT y -> Array.get !array_vars y;
            | _ -> raise (Invalid_argument "Should never happen")
        end
    | NAME x -> INT 0 (* We don't actually want to do anything bc its a label*)
        
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
    | MOVE (x1, x2) -> 
        begin
        match x1 with
            | TEMP y -> let ans = interp_expr_val x2 in
                Hashtbl.add !vars y ans;
                INT 0
            | MEM_TEMP y -> let ans = interp_expr_val x2 in
                let y' = interp_expr_val y in
                    begin
                    match y' with
                    | INT z -> begin
                        match ans with
                        | INT a -> Array.set !array_vars z ans;
                                    INT 0
                        | _ -> raise (Invalid_argument "Should never happen")
                        end   
                    | _ -> raise (Invalid_argument "Should never happen")
                    end
                
            (* | NAME y1 -> 
            begin
                match x2 with
                If NAME is on the left, then it also needs to be on the right.
                It should copy the data.
                | NAME y2 -> 
                begin
                    
                end
                | _ -> raise (Invalid_argument "Should never happen")
            end *)
            | _ -> raise (Invalid_argument "Should never happen")
        end
    | COPY (t, x1) ->
        begin
            let ans = interp_expr_val x1 in
                Hashtbl.add !vars t ans;
                INT 0
        end
    | EXP (x) -> interp_expr x;
                INT 0
    | SEQ (s1, s2) -> 
        begin
            let temp = interp_stm s1 jmp whole in
            if (!glblJmp = jmp)
                then (interp_stm s2 jmp whole)
                else temp
        end
    (* | CALL (f, xprLst) -> begin
        match f with
        | NAME x -> 
            let rest = look_for_label l whole whole in
            begin
                shove_args xprLst;
                Hashtbl.add !vars 0 func_args
                Hashtbl.add !vars 1 x
                glblJmp := !glblJmp + 1;
                interp_expr rest (jmp+1) whole
                (* Return the correct value *)
                Hashtbl.find !vars 1 
            end
        | _ -> raise (Invalid_argument "Should never happen. Functions should be places")
    end *)
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
    | LABEL (l) -> INT 0
    | ALLOC_MEM (temp, i) -> begin
        (* Everything is an array. even singletons *)
        Hashtbl.add !vars temp (INT !stack_pointer);
        (* Add to the stack pointer *)
        stack_pointer := (!stack_pointer) + i;
        INT !stack_pointer
        end
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
    | NAME z1 -> if (val1 = z1) then true else false
    | _ -> raise (Invalid_argument "Should never happen")
(* and shove_args xprLst =
    match xprLst with
    | (xs::xss) -> begin 
            Array.set func_args !arg_number xs;
            arg_number := !arg_number + 1;
            shove_args xss
    | [] -> INT 0 *)