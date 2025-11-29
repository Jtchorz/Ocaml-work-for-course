open Parser
open Printf
open Lexer
open Ast




type func_sig = identifier*type_id*(type_id list) (* name, return type, parameter types*)
type var_key = int (* every variable gets a unique key assigned to it *)
type var_map = identifier * var_key (* map the name to the key, so accessed variables are converted correctly *)
type scope = 
  | Global of var_map list 
  | Nested of var_map list * scope (* reference to parent scope *)

type label = string (* defines the assembly labels, like where a global variable is found *)
type block_key = int (* each block (in a function) gets a unique key assigned to it *)
type bitsize = Byte | Word | DWord | QWord (* size of a variable *)

(* the environment is a data record that gets passed along during the conversion and updated continuously *)
(* it holds variable mappings, labels for functions and global variables, the size of variables, what the next keys to assign are, etc *)
type environment = {
  global_vars_labels : (var_key*label*int option) list ; (* key, label and initial value *)  
  current_scope : scope ;
  var_key_to_size : (var_key * type_id ) list ;
  next_block_id : block_key ; 
  next_var_id : var_key ; 
  extern_funcs: identifier list ;
  function_param_sizes : (identifier* bitsize list) list
}

(* need to change expression type to use symbols (var_key) instead *)
type ir_expr = 
  | IREVar of var_key
  | IREInt of int
  | IREChar of char
  | IREString of string
  | IREUnOp of unary_op*ir_expr 
  | IREBinOp of binary_op*ir_expr*ir_expr
  | IRECall of label*(ir_expr list)
  | IRENew of type_id*ir_expr
  | IREArrayAccess of var_key * ir_expr

type ir_stmt = 
  | IRSCall of ir_expr
  | IRSVarDeclare of type_id * var_key
  | IRSVarAssign of var_key * ir_expr
  | IRSDelete of var_key 
  | IRSArrayAssign of var_key * ir_expr * ir_expr (* array, index_expr, value_expr *)

type ir_end = 
  | IRSReturn of ir_expr option
  | IRSBranch of ir_expr * block_key * block_key
  | IRSJump of block_key
  | IRSNoOp (* used for empty blocks, like those after if-statements *)

type ir_basic_block = block_key * (ir_stmt list * ir_end)

type ir_global = 
  | IRFunc of identifier * type_id * var_key list * ir_basic_block list
  | IRDecl  (* a function declaration is handled in the environment, but is noted in the global list for pprinting purposes *)
  | IRVarDef (* similar to above *)
  | IRVarDecl


(* get the next block/variable id and update environment accordingly *)
let inc_block_id ir_env = (ir_env.next_block_id, {ir_env with next_block_id = ir_env.next_block_id + 1})  
let inc_var_id ir_env = (ir_env.next_var_id, {ir_env with next_var_id = ir_env.next_var_id + 1})

(* map identifier -> var_key *)
let find_var_key ir_env id = 
  let rec iterate_list lst = match lst with 
    | [] -> None 
    | (h,key)::tail when h = id -> Some(key)
    | _::tail -> iterate_list tail in 
  let rec iterate_scope scope = match scope with (* should return the var_key of the matching identifier in the inner-most scope, because of shadowing *)
    | Global(lst) -> iterate_list lst 
    | Nested(lst,s) -> match iterate_list lst with 
      | None -> iterate_scope s 
      | Some(x) -> Some(x) in 
  match iterate_scope ir_env.current_scope with 
  | None -> failwith "VARIABLE NAME NOT FOUND WHILE ACCESSING IT" (* should not happen after name checking *)
  | Some(x) -> x

  (* define how large each data type is *)
let type_to_size = function 
  | TInt -> QWord
  | TChar -> Byte 
  | TPoint(_) -> QWord
let var_size t = match t with 
  | QWord -> 8
  | Byte -> 1

(* add a variable to the current scope, with the specified var_key key *)
let env_add_var ir_env var key tpe = {ir_env with current_scope = (match ir_env.current_scope with 
                                | Global(lst) -> Global((var,key)::lst)
                                | Nested(lst,s) -> Nested((var,key)::lst, s)) ; var_key_to_size=(key, tpe)::ir_env.var_key_to_size
                                }

(* add a function, with the specified func_key key *)
let env_add_func ir_env key param_sizes =
  {ir_env with function_param_sizes=(key,param_sizes)::ir_env.function_param_sizes}

(* exit a scope, forgetting the variables defined within it *)
let pop_scope ir_env = let s = match ir_env.current_scope with 
  |Nested(_,s) -> s in 
  {ir_env with current_scope = s}

  (* AST to CFG-IR*)

(* transform into the var_key based expression type. also removes metadata and actually turns characters into chars *)
let rec clean_expr (_,expr) ir_env = match expr with 
  | EVar((_,identifier)) -> IREVar(find_var_key ir_env identifier)
  | EInt(n) -> IREInt(n)
  | EBinOp((_,binary_op),expr1,expr2) -> IREBinOp(binary_op, clean_expr expr1 ir_env, clean_expr expr2 ir_env)
  | EChar(c) -> let cc = match c with 
      | "\\n" -> Char.chr 10
      | "\\t" -> Char.chr 9
      | "\\\\" -> Char.chr 92
      | "\\\'" -> Char.chr 39
      | "\\\"" -> Char.chr 34
      | _ -> c.[0] in IREChar(cc)
  | EString(s) -> IREString(s)
  | EUnOp((_,unary_op),expr1) -> IREUnOp(unary_op, clean_expr expr1 ir_env) 
  | ECall ((_,identifier), exprs) -> IRECall(identifier, List.map (fun e -> clean_expr e ir_env) exprs)
  | ENew ((_,type_id),expr1) -> IRENew(type_id, clean_expr expr1 ir_env)
  | EArrayAccess((_,id),index_expr,_) -> IREArrayAccess(find_var_key ir_env id, clean_expr index_expr ir_env)

(* In order to not have to run any setup assembly code to properly instantiate global variables, I compute their value here. *)
(* These values must be constant, so I don't worry about the IREVar or IRECall expressions, and I also only handle integers. *)
let rec precompute_global_variable expr  = 
  let compute_unary op n = match op with 
  | LogNotOp -> lnot n
  | BinNotOp -> max_int lxor n 
  | UnaryMinOp -> -n
in
  let compute_binary op n1 n2 = match op with 
  | PlusOp-> n1+n2
  | MinusOp -> n1-n2 
  | MultOp -> n1*n2 
  | DivOp -> n1/n2 
  | ModOp -> n1 mod n2 
  | LTOp -> if n1 < n2 then 1 else 0 
  | GTOp -> if n1 > n2 then 1 else 0 
  | LEQOp -> if n1 <= n2 then 1 else 0 
  | GEQOp -> if n1>=n2 then 1 else 0 
  | EqOp -> if n1==n2 then 1 else 0 
  | NeqOp -> if n1!=n2 then 1 else 0 
  | BinAndOp -> n1 land n2
  | BinOrOp -> n1 lor n2 
  | LogAndOp -> if (n1!=0) && (n2!=0) then 1 else 0 
  | LogOrOp -> if (n1!=0) || (n2!=0) then 1 else 0 
  | ShiftLeftOp -> n1 lsl n2
  | ShiftRightOp -> n1 lsr n2
in
  match expr with 
  | IREVar(_) -> failwith "INITIALIZER ELEMENT IS NOT A CONSTANT"
  | IREInt(n) -> n
  | IREChar(c) -> failwith "MUST BE INT"
  | IREString(s) -> failwith "MUST BE INT"
  | IREUnOp(unary_op,e) -> compute_unary unary_op (precompute_global_variable e)
  | IREBinOp(binary_op,e1,e2) -> compute_binary binary_op (precompute_global_variable e1) (precompute_global_variable e2)
  | IRECall(_) -> failwith "INITIALIZER ELEMENT IS NOT A CONSTANT"
  | IRENew(_) -> failwith "MUST BE INT"

(*
this returns a list of constructed statements (not yet put into a block), a list of constructed blocks, and an updated environment
*)
let rec stmts_to_cfg tree acc_stmts ir_env= 
  let rec stmt_to_cfg stmt acc_stmts ir_env = 
    let (_, t) = stmt in 
    match t with
    (* only SExpr allowed is the ECall, so I can safely put it into IRSCall *)
    | SExpr(expression) -> (IRSCall(clean_expr expression ir_env)::acc_stmts, [], ir_env)

    | SVarAssign ((_,identifier), expression) -> 
      let var_id = find_var_key ir_env identifier in 
      (IRSVarAssign(var_id, clean_expr expression ir_env)::acc_stmts, [], ir_env)

    | SScope (stmts_inner) -> (* pushes a scope onto the scope stack *)
      let (ir_stmts,ir_blocks, env2) = stmts_to_cfg stmts_inner acc_stmts {ir_env with current_scope=Nested([], ir_env.current_scope)} in 
      (ir_stmts,ir_blocks, pop_scope env2) (* pop the scope on the returned environment *)

    | SVarDef ((_,t), (_,identifier),expression) -> 
      let (var_id, ir_env2) = inc_var_id ir_env in (* get a new var_key *)
      let ir_env2 = env_add_var ir_env2 identifier var_id t in (* add to environment *)
      (IRSVarAssign(var_id, clean_expr expression ir_env)::IRSVarDeclare(t,var_id)::acc_stmts, [], ir_env2) (* split into two separate steps *)

    | SReturn(None) -> 
      let (bid, ir_env2) = inc_block_id ir_env in (* finish of a block *)
      ([], [(bid, (List.rev acc_stmts, IRSReturn(None)))],ir_env2) (* return the list of blocks with stmts in right order *)

    | SReturn(Some(e)) -> 
      let (bid, ir_env2) = inc_block_id ir_env in (* finish of a block *)
      ([], [(bid, (List.rev acc_stmts, IRSReturn(Some(clean_expr e ir_env))))],ir_env2) (* return the list of blocks with stmts in right order *)

    | SIf (expression,statement, else_stmt_opt) -> 
      let ir_expr = clean_expr expression ir_env in 
      let (end_bid, ir_envt) = inc_block_id ir_env in 
      let (if_end_bid, ir_env2) = inc_block_id ir_envt in 
      let (if_instrs,if_blockst,ir_env3) = stmt_to_cfg statement [] ir_env2 in 
      let if_blocks = if_blockst @ [(if_end_bid, (List.rev if_instrs, IRSJump(end_bid)))] in
      let if_start_bid = ( (* get the block_id of the first block in the if-clause *)
        match if_blocks with 
          | [] -> failwith "LIST CANNOT BE EMPTY HERE" (* since I just added a block to the list *)
          | (bid,_)::_ -> bid 
      ) in 
      let (else_blocks, else_start_bid, final_env) = (match else_stmt_opt with
      | None -> ([], end_bid, ir_env3)
      | Some(else_branch) -> (
        let (else_start_bid, ir_envt) = inc_block_id ir_env3 in 
        let (else_end_bid, ir_env2) = inc_block_id ir_envt in 
        let (else_instrs,else_blockst,ir_env3) = stmt_to_cfg else_branch [] ir_env2 in 
        let else_blocks = else_blockst @ [(else_end_bid, (List.rev else_instrs, IRSJump(end_bid)))] in 
        let else_start_bid = (
        match else_blocks with 
          | [] -> failwith "LIST CANNOT BE EMPTY HERE" (* since I just added a block to the list *)
          | (bid,_)::_ -> bid 
        ) in 
        (else_blocks, else_start_bid, ir_env3)
      )) in 
      let (first_bid, final_env2) = inc_block_id final_env in 
      
      ([], ((first_bid, (List.rev acc_stmts, IRSBranch(ir_expr, if_start_bid, else_start_bid)))::if_blocks) @ else_blocks @ [(end_bid, ([], IRSNoOp))], final_env2)

    | SWhile (expression, while_body_stmt) -> 
      let ir_expr = clean_expr expression ir_env in 
      let (before_bid, env2)  = inc_block_id ir_env in 
      let (while_start, env3)  = inc_block_id env2 in 
      let before_block = (before_bid, (List.rev acc_stmts, IRSJump(while_start))) in 
      let (while_instrs,while_blockst,env4) = stmt_to_cfg while_body_stmt [] env3 in 
      let (while_end, env5) = inc_block_id env4 in 
      let final_while_block =  (while_end, (List.rev while_instrs, IRSJump(while_start))) in 
      let while_blocks = while_blockst @ [final_while_block] in 
      let first_while_bid = (
        match while_blocks with 
        | [] -> failwith "LIST CANNOT BE EMPTY HERE" (* since I just added a block to the list *)
        | (bid,_)::_ -> bid 
      ) in 
      let (after_while, env6) = inc_block_id env5 in 
      let post_block = (after_while, ([], IRSNoOp)) in 
      let cond_block = (while_start, ([], IRSBranch(ir_expr, first_while_bid, after_while))) in 
      ([], before_block :: cond_block :: (while_blocks @ [post_block]),env6) 
      
    | SDelete ((_,identifier)) -> let var_id = find_var_key ir_env identifier in 
      (IRSDelete(var_id)::acc_stmts, [], ir_env) 

    | SArrayAssign((_,identifier), index_expr, _, expression) ->  (* I will not do structs, so ignore field name *)
      let ir_index = clean_expr index_expr ir_env in
      let ir_assign = clean_expr expression ir_env in
      let var_id = find_var_key ir_env identifier in 
      (IRSArrayAssign(var_id, ir_index, ir_assign)::acc_stmts, [], ir_env) in

    (* | SBreak -> (AnalysisSuccess, env)*) (* not implemented *)

    let rec eat_stmts lst ir_env block_stmts blocks = match lst with 
    | [] -> (block_stmts ,blocks, ir_env)
    | h::tail -> 
      let (ir_stmts,ir_blocks, env2) = stmt_to_cfg h block_stmts ir_env in
      eat_stmts tail env2 ir_stmts (blocks @ ir_blocks)  in
    let (ir_stmts, ir_blocks, ir_env) = eat_stmts tree ir_env acc_stmts [] in 
    (ir_stmts, ir_blocks, ir_env)

(* returns a list of IRGlobal and an environment *)
let global_to_cfg tree ir_globals ir_env =  
  (* handles adding function parameters to the environment *)
  let rec add_parameters params acc acc_size env = match params with 
    | [] -> (List.rev acc, List.rev acc_size, env)
    | (_,Param((_,t),(_,p)))::tail -> 
      let (var_id, env2) = inc_var_id env in
      add_parameters tail (var_id::acc) ((type_to_size t)::acc_size) (env_add_var env2 p var_id t) 
  in
  let (_,t) = tree in 
  match t with
  | GFuncDef(type_id,(_,identifier),parameters, stmt)  -> 
    let func_env = {ir_env with current_scope=Nested([], ir_env.current_scope)} in (* push the function scope *)
    let (param_keys, param_sizes, env2) = add_parameters parameters [] [] func_env in 
    let envP = env_add_func env2 identifier param_sizes in (* add function to environment, making it callable *)
    let (ir_instr,ir_blocks, envp) = stmts_to_cfg [stmt] [] envP in (* convert statements to IRStmt *)
    let (complete_blocks, envpp) = (
      match ir_instr with (* if a function does not end with a return, add a return block. for simplicity I always add a return block, which is sometimes never executed because it follows another return block *)
      | _ -> let (bid,env2) = inc_block_id envp in 
                        (ir_blocks @ [(bid, (List.rev ir_instr, IRSReturn(None)))], env2)
      (* | [] ->  (ir_blocks, envp) *)
    ) in 
    (IRFunc(identifier, snd type_id, param_keys, complete_blocks), envpp)
  | GFuncDecl(_, (_,id),parameters) -> 
    let func_env = {ir_env with current_scope=Nested([], ir_env.current_scope)} in (* make environment that allows parameter data extraction *)
    let (_, param_sizes, env2) = add_parameters parameters [] [] func_env in 
    (IRDecl, {(pop_scope env2) with extern_funcs=id::ir_env.extern_funcs; function_param_sizes=(id, param_sizes)::ir_env.function_param_sizes})
  | GVarDef((_,tpe),(_,identifier),expression) -> 
    let n = precompute_global_variable (clean_expr expression ir_env) in (* get the constant value to assign *)
    let (var_id, env2) = inc_var_id ir_env in
    let env3 = env_add_var env2 identifier var_id tpe in (* add to global scope *)
    (IRVarDef, {env3 with global_vars_labels=(var_id,sprintf "glob_var%d" var_id, Some(n))::env3.global_vars_labels}) 
  (* I passed all tests without implementing external global variables, but I add the below for completeness *)
  (* they are implemented by adding their labels to extern_funcs, and setting their intial value to None, which is used to ensure that they are not printed in .data *)
  | GVarDecl((_,tpe),(_,identifier)) -> 
    let (var_id, env2) = inc_var_id ir_env in
    let env3 = env_add_var env2 identifier var_id tpe in (* add to global scope *)
    (IRVarDef, {env3 with global_vars_labels=(var_id,identifier, None)::env3.global_vars_labels; extern_funcs=identifier::ir_env.extern_funcs}) 
   
(* returns a list of globals and the final environment *)
let ast_to_cfg tree = 
  let rec convert prog ir_globals ir_env = 
  match prog with 
  | GlobTerminal -> (List.rev ir_globals, ir_env)
  | ConsGlob(_,glob, program) -> 
      let (ir_glob2, ir_scope2) = global_to_cfg glob ir_globals ir_env in
      convert program (ir_glob2::ir_globals) ir_scope2 in
  convert tree [] {
    global_vars_labels = [] ;  
    current_scope = Global([]) ;
    var_key_to_size = [] ;
    next_block_id = 1; (* zero is reserved for the parameter block, handled when crafting assembly code *)
    next_var_id = 0;
    extern_funcs = [] ;
    function_param_sizes = []
    }


