(* Definition of the abstract syntax for scrawl. The parser builds up a tree
   of type abstract_syntax_tree as it goes through the input program and parses
   it according to the concrete syntax. The abstract syntax tree is what will be
   used in syntactic analysis *)

(* Many nodes of the abstract syntax tree will hold the position of where the 
   node came from in the original file in order to better report errors *)
type pos = int

(* A complete program is just a list of expressions to be sequentially evaluated *)
type abstract_syntax_tree = AST of expr_list
(* An expression is any of the many basic elements of a program *)
and expr = 
    | VarExpr of var
    | DeclExpr of decl
    | AssignExpr of assign
    | LambdaExpr of lambda
    | ReturnExpr of expr
    | IntLitExpr of int_lit
    | FloatLitExpr of float_lit
    | StringLitExpr of string_lit
    | BoolLitExpr of bool_lit
    | FuncCallExpr of func_call
    | BinOpExpr of bin_op_expr
    | UnOpExpr of un_op_expr
    (* Control flow is C style *)
    | IfExpr of if_expr
    | ForExpr of for_expr
    | WhileExpr of while_expr
and var = 
    | SimpleVar of simple_var 
    | ArrayVar of array_var
and decl = 
    | SimpleDecl of simple_decl
    | ArrDecl of arr_decl
    | FuncDecl of func_decl
(* Good god why doesn't ocaml have anonymous record types *)
and assign = {var : var; value : expr; pos : pos}
and lambda = {params: param list; body: expr; pos: pos}
and int_lit = {value: int; pos: pos}
and float_lit = {value: float; pos: pos}
and string_lit = {value: string; pos: pos}
and bool_lit = {value: bool; pos: pos}
and func_call = {func: string; args: expr_list; pos: pos}
and bin_op_expr = {op: bin_op; argl: expr; argr: expr; pos: pos}
and un_op_expr = {op: un_op; arg: expr; pos: pos}
and if_expr = {cond: expr; body: expr_list; else_expr: expr_list; pos: pos}
and for_expr = {iter_var: expr; cond: expr; iter: expr; body: expr_list; pos: pos}
and while_expr = {cond: expr; body: expr_list; pos: pos}
and simple_var = {ident: string; pos: pos} (* An ordinary identifier *)
and array_var = {arr: var; idx: expr; pos: pos} (* An array along with an index *)
and simple_decl = {var_type: scrawl_type; ident: string; pos: pos}
and arr_decl = {arr_type: scrawl_type; ident: string; len: int; pos: pos}
and func_decl = {ret_type: scrawl_type; ident: string; params: param list; pos: pos}
and bin_op = 
    | BAND | BOR | BXOR | BLEFT | BRIGHT 
    | LAND | LOR | LXNOR | LXAND | LXNAND 
    | EQ | LESS | GREATER 
    | PLUS | MINUS | TIMES | DIV | MOD | POW
and un_op = 
    | BNOT | LNOT | UMINUS
and scrawl_type =
    | INT | FLOAT | BOOL | STRING | ScrawlArrayType of scrawl_array_type
and scrawl_array_type = {array_type: scrawl_type; len: int; pos: pos}
and param = QualIdent of qual_ident
and qual_ident = {ident_type: scrawl_type; ident: string; pos: pos}
and expr_list = ExprLst of expr * expr_list | None

