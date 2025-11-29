open Cfg
open Printf
open Ast


(* ----------------- CFG to Assembly ------------------*)

type stack_frame_node = Frame of int * stack_frame_node option

(* the environment for the assembly generation. includes things like where in the stack a certain variable will be stored, labels for string literals, etc.  *)
type asm_env  = {
  sp_offset : int;
  stack_frame_offsets : stack_frame_node;
  var_stack_offsets : (var_key * int) list ;
  var_id_to_size : (var_key * type_id) list ; 
  func_param_sizes : (identifier* (bitsize list)) list ; 
  string_labels : (string * string) list ; (*string->label*)
  next_string_label : int  ;
  global_vars_labels : (var_key*label*int option) list ; (* key, label and initial value *)  
}

(* maps a var_key to its size. not efficient, could be replaced with e.g. a hashmap *)
let get_var_length x env = 
  let rec work x lst = match lst with 
    | (h,s)::tail when x=h -> type_to_size s
    | _::tail -> work x tail in 
  work x env.var_id_to_size

(* maps a pointer variable to its pointed size. not efficient, could be replaced with e.g. a hashmap *)
let get_pointed_size x env = 
  let rec work x lst = match lst with 
    | (h,s)::tail when x=h -> s
    | _::tail -> work x tail in 
  match work x env.var_id_to_size with 
  | TPoint(_,t) -> type_to_size t 
  | _ -> failwith "THIS SHOULD HAVE BEEN A POINTER" (* should not happen after typechecking *)


let get_var_size x env = 
  var_size (get_var_length x env)

(* maps a parameter to its size. not efficient, could be replaced with e.g. a hashmap *)
let find_param_sizes env id =
  let rec work lst id = match lst with 
    | (h,ps)::tail when h=id -> ps 
    | _::tail -> work tail id in 
  work env.func_param_sizes id 

type asm_unops = Inc | Dec | Push | Pop | Mul | Div | SetLt | SetGt |
                  SetLeq | SetGeq | SetEq | SetNeq | BitNot | Neg
type asm_binops = Add | Sub | Mov | Cmp | BinAnd | BinOr 
                  | ShiftLeft | ShiftRight | MovZx


type reg = RAX | RBX  | RCX | RDX | RSI | RDI | RBP | RSP | R8 | R9 | R10 | R11 | R12 |R13 | R14 | R15
(* specify what specific register should be used, including its size *)
type reg_ext = reg * bitsize
(* used for function parameters *)
let arg_registers = [RDI;RSI;RDX;RCX;R8;R9]

type asm_operands = 
    | Imm of int 
    | Register of reg_ext
    | MemoryAddress of bitsize * reg * reg option * int * int (* bitsize[reg+int_1*reg_opt+int_2] *)
    | Label of string 

(* I only use Jne, but this list could of course be extended, likely leading to more optimized assembly *)
type jbinop = Jl | Jg | Jne

type asm_blockend = 
    | Ret 
    | Jmp of block_key
    | Branch of jbinop * block_key * block_key
    | NoOp

type asm_instr = 
    | UnaryOp of asm_unops * asm_operands
    | BinaryOp of asm_binops * asm_operands * asm_operands
    | Call    of identifier (* call a function specifically *)

type asm_block = AsmBlock of block_key * (asm_instr list * asm_blockend)
type asm_func = AsmFunc of identifier * asm_block list 

let push_int env = (env.sp_offset+8, {env with sp_offset = env.sp_offset + 8})
let pop_int env = (env.sp_offset-8, {env with sp_offset = env.sp_offset - 8})

let push_n env n = (env.sp_offset+n, {env with sp_offset = env.sp_offset + n})
let pop_n env n = (env.sp_offset-n, {env with sp_offset = env.sp_offset - n})

(* push a variable onto the environment stack, and note down its offset *)
let add_var env var_id offset = {env with var_stack_offsets=(var_id,offset)::env.var_stack_offsets}
(* get the offset at which a variable exists *)
let get_offset env id = 
  let rec work lst = match lst with 
    | [] -> None (* implies that the variable is a global variable, i.e. not on the stack *)
    | (h,o)::tail when h = id -> Some(o)
    | _::tail -> work tail in 
  work env.var_stack_offsets

(* pop the stack frame when exiting a function *)
let pop_frame env = 
  let (offset, parent_frame) =  ( match env.stack_frame_offsets with
    | Frame(offset, Some(f)) -> (offset, f)
  ) in 
  (env.sp_offset, {env with sp_offset=offset; stack_frame_offsets=parent_frame})
(* push a stack frame when entering a function *)
let push_frame env = {env with stack_frame_offsets=Frame(env.sp_offset, Some(env.stack_frame_offsets))}

(* used to zip parameter list with parameter register list *)
(* does not work for varargs functions *)
let shear_zip lst1 lst2 = 
  let rec work lst1 lst2 acc = match lst1,lst2 with 
    | [],_ |_,[] -> List.rev acc
    | h1::tail1, h2::tail2 -> work tail1 tail2 ((h1,h2)::acc) in 
  work lst1 lst2 []

(* get the label where a string is stored. if it does not exist, create one and add to environment *)
(* this way, if the same string literal appears twice, it will only be stored once *)
(* and since I store them in .rodata, this ensures correctness *)
let get_string_label env s =
  (* either, we find that the string is already stored (in which case we return that label), or we add it *)
  let rec work lst = match lst with 
    | [] -> 
      let lbl = sprintf "glob_string_lbl%d" env.next_string_label in 
      (lbl, {env with next_string_label=env.next_string_label+1 ; string_labels = (s,lbl)::env.string_labels})
    | (str,lbl)::_ when s = str -> (lbl,env)
    | _::tail -> work tail in 
  work env.string_labels

let get_global_var_lbl env var_id = 
  let rec work lst = match lst with 
  | [] -> failwith "GLOBAL VARIABLE IS ALWAYS FOUND HERE" (* does not happen after name analysis *)
  | (vid,lbl,_)::_ when vid=var_id -> lbl
  | _::tail -> work tail in 
  work env.global_vars_labels

(* Map the binary operations to the instructions necessary to perform them *)
(* They assume values in RAX and RDX, and leaves the result in RAX *)
(* Even if the operands are smaller than QWord (like chars), this assumes that the upper bytes are zeroed, so comparisons happen correctly *)
let binop_to_instrs binop = match binop with 
  | PlusOp -> [BinaryOp(Add, Register(RAX, QWord), Register(RDX, QWord))]
  | MinusOp -> [BinaryOp(Sub, Register(RAX, QWord), Register(RDX, QWord))]
  | MultOp -> [UnaryOp(Mul, Register(RDX, QWord))]
  | DivOp -> [BinaryOp(Mov, Register(RSI,QWord), Register(RDX,QWord));BinaryOp(Mov, Register(RDX,QWord), Imm(0)); UnaryOp(Div, Register(RSI, QWord))]
  | ModOp -> [BinaryOp(Mov, Register(RSI,QWord), Register(RDX,QWord));BinaryOp(Mov, Register(RDX,QWord), Imm(0)); UnaryOp(Div, Register(RSI, QWord)) ; BinaryOp(Mov, Register(RAX,QWord), Register(RDX,QWord))]
  | LTOp -> [BinaryOp(Cmp, Register(RAX, QWord), Register(RDX, QWord)); UnaryOp(SetLt, Register(RAX,Byte)); BinaryOp(MovZx, Register(RAX,QWord), Register(RAX,Byte))]
  | GTOp -> [BinaryOp(Cmp, Register(RAX, QWord), Register(RDX, QWord)); UnaryOp(SetGt, Register(RAX,Byte)); BinaryOp(MovZx, Register(RAX,QWord), Register(RAX,Byte))]
  | LEQOp -> [BinaryOp(Cmp, Register(RAX, QWord), Register(RDX, QWord)); UnaryOp(SetLeq, Register(RAX,Byte)); BinaryOp(MovZx, Register(RAX,QWord), Register(RAX,Byte))]
  | GEQOp -> [BinaryOp(Cmp, Register(RAX, QWord), Register(RDX, QWord)); UnaryOp(SetGeq, Register(RAX,Byte)); BinaryOp(MovZx, Register(RAX,QWord), Register(RAX,Byte))]
  | EqOp -> [BinaryOp(Cmp, Register(RAX, QWord), Register(RDX, QWord)); UnaryOp(SetEq, Register(RAX,Byte)); BinaryOp(MovZx, Register(RAX,QWord), Register(RAX,Byte))]
  | NeqOp -> [BinaryOp(Cmp, Register(RAX, QWord), Register(RDX, QWord)); UnaryOp(SetNeq, Register(RAX,Byte)); BinaryOp(MovZx, Register(RAX,QWord), Register(RAX,Byte))]
  | BinAndOp -> [BinaryOp(BinAnd, Register(RAX,QWord), Register(RDX,QWord))]
  | BinOrOp ->  [BinaryOp(BinOr, Register(RAX,QWord), Register(RDX,QWord))]
  | LogAndOp -> [BinaryOp(Cmp, Register(RAX,QWord), Imm(0)); UnaryOp(SetNeq, Register(RSI, Byte)) ; BinaryOp(MovZx, Register(RAX,QWord), Register(RSI, Byte)); BinaryOp(Cmp, Register(RDX,QWord), Imm(0)); UnaryOp(SetNeq, Register(RSI, Byte)) ; BinaryOp(MovZx, Register(RDX,QWord), Register(RSI, Byte)); BinaryOp(BinAnd, Register(RAX,QWord), Register(RDX,QWord))]
  | LogOrOp ->  [BinaryOp(Cmp, Register(RAX,QWord), Imm(0)); UnaryOp(SetNeq, Register(RSI, Byte)) ; BinaryOp(MovZx, Register(RAX,QWord), Register(RSI, Byte)); BinaryOp(Cmp, Register(RDX,QWord), Imm(0)); UnaryOp(SetNeq, Register(RSI, Byte)) ; BinaryOp(MovZx, Register(RDX,QWord), Register(RSI, Byte)); BinaryOp(BinOr, Register(RAX,QWord), Register(RDX,QWord))]
  | ShiftLeftOp -> [BinaryOp(Mov, Register(RCX,Byte),Register(RDX,Byte)); BinaryOp(ShiftLeft, Register(RAX,QWord), Register(RCX,Byte))]
  | ShiftRightOp -> [BinaryOp(Mov, Register(RCX,Byte),Register(RDX,Byte));  BinaryOp(ShiftRight, Register(RAX,QWord), Register(RCX,Byte))]

let upper_bound_size s1 s2 = match s1, s2 with 
  | Byte,Byte -> Byte 
  | _,_-> QWord (* since I don't use sizes other than Byte and QWord *)

let unop_to_instrs unop = match unop with 
  | LogNotOp -> [BinaryOp(Cmp, Register(RAX, QWord), Imm(0)); UnaryOp(SetEq, Register(RSI,Byte)); BinaryOp(MovZx, Register(RAX,QWord), Register(RSI,Byte))]
  | BinNotOp -> [UnaryOp(BitNot, Register(RAX,QWord))]
  | UnaryMinOp -> [UnaryOp(Neg, Register(RAX,QWord))]

(* returns a list of instructions, the size of the result from the expression, and an environment *)
(* the result of an expression is given in RAX, whose size is given by the second return value *)
(* the list of instructions have the invariant that after they have been computed, RSP will have the value it had prior to them being computed *)
let rec expr_to_instrs expr env = 
  (* handle parameters in a function call, i.e. compute their values and put into the right registers *)
  let rec params_to_instrs arg_reg_list acc1 acc2 env bytes_to_pop = match arg_reg_list with 
  | [] -> let (_,final_env) = pop_n env bytes_to_pop in (List.rev_append acc1 acc2 , final_env )
  | ((e,s),r)::tail -> (
    let (instr, _,env1) = expr_to_instrs e env in 
    let acc1p = (UnaryOp(Push, Register(RAX,s)))::(List.rev_append instr acc1) in
    let (_,env2) = push_n env1 (var_size s) in 
    let acc2p = (UnaryOp(Pop, Register(r,s)))::acc2 in 
    params_to_instrs tail acc1p acc2p env2 (bytes_to_pop + (var_size s))
  ) in
  match expr with 
  | IREVar(var_id) -> 
    let size = get_var_length var_id env in 
    (match get_offset env var_id with 
    | Some(offset) -> ([BinaryOp(Mov, Register(RAX,size), MemoryAddress(size, RSP, None, 0, env.sp_offset - offset))], size, env) (* access the variable by its offset from the current RSP *)
    | None -> (* the variable is not found on the stack, so it must be global *)
      let lbl = get_global_var_lbl env var_id in 
      ([BinaryOp(Mov, Register(RDX,QWord), Label(lbl)); BinaryOp(Mov, Register(RAX,size), MemoryAddress(size, RDX, None, 0, 0))], size, env))
  | IREInt(n) -> 
    ([BinaryOp(Mov, Register(RAX, QWord), Imm(n))],QWord, env)
  | IREBinOp(binary_op,expr1,expr2) ->
    let (instr1,s1, env1) = expr_to_instrs expr2 env in 
    let (_,intermed_env) = push_int env1 in 
    let (instr2, s2,env2) = expr_to_instrs expr1 intermed_env in 
    let (_,final_env) = pop_int env2 in 
    (* instr1 :: PUSH $RAX :: instr2 :: POP $RDX :: BINOP $RAX $RDX *)
    (instr1 @ (UnaryOp(Push, Register(RAX, QWord)) :: instr2) @ (UnaryOp(Pop, Register(RDX, QWord))::(binop_to_instrs binary_op)), upper_bound_size s1 s2, final_env)
  | IREUnOp(unary_op, expr1) -> 
    let (instrs,size, env1) = expr_to_instrs expr1 env in 
    (instrs@(unop_to_instrs unary_op), size, env1)
  | IRECall (label,ir_exprs) -> 
      let param_sizes = find_param_sizes env label in 
      if List.length ir_exprs = List.length param_sizes then (* normal, reasonable case. which is not a varargs function *)
      (
        let (param_instrs, env1) = params_to_instrs (shear_zip (List.combine ir_exprs param_sizes) arg_registers) [] [] env 0 in 
        (param_instrs @ [BinaryOp(Mov, Register(RAX, QWord), Imm(0));Call(label)], QWord, env1) (* clear RAX for printf *)
      )
      else (* awful, mean case. which is a varargs function *)
      (
        let arg_sizes = List.init (List.length ir_exprs) (fun _ -> QWord) in 
        let (param_instrs, env1) = params_to_instrs (shear_zip (List.combine ir_exprs arg_sizes) arg_registers) [] [] env 0 in 
        (param_instrs @ [BinaryOp(Mov, Register(RAX, QWord), Imm(0));Call(label)], QWord, env1) (* clear RAX for printf *)
      )
  | IREChar(c) -> 
    ([BinaryOp(Mov, Register(RAX, QWord), Imm( Char.code c ))],Byte, env)
  | IREString(s) -> 
    let (lbl,env1) = get_string_label env s in (* add label to environment *)
    ([BinaryOp(Mov, Register(RAX, QWord), Label(lbl))], QWord, env1)
  | IRENew (type_id,expr) -> 
    let (instr1,_, env1) = expr_to_instrs expr env in (* RAX will hold the length of array that should be made *)
    let size = type_to_size type_id in 
    (instr1 @ [(BinaryOp(Mov, Register(RDX, QWord), Imm(var_size size))); UnaryOp(Mul, Register(RDX,QWord)); BinaryOp(Mov, Register(RDI, QWord), Register(RAX,QWord)); Call("malloc")],size, env1)
  | IREArrayAccess(var_id, ir_expr) -> 
    let (instr1,_, env1) = expr_to_instrs ir_expr env in (* RAX will hold the index of the element to be accessed *)
    let size = get_pointed_size var_id env in 
    (match get_offset env1 var_id with 
    | Some(offset) -> 
      (instr1 @ [BinaryOp(Mov, Register(RDX,QWord), MemoryAddress(QWord, RSP, None, 0, env1.sp_offset - offset)); BinaryOp(Mov, Register(RAX, size), MemoryAddress(size, RDX, Some(RAX), var_size size, 0))], size, env1)
    | None -> (* the variable is not found on the stack, so it must be global *)
      let lbl = get_global_var_lbl env1 var_id in 
      ([BinaryOp(Mov, Register(RDX,QWord), Label(lbl)); BinaryOp(Mov, Register(RAX,size), MemoryAddress(size, RDX, Some(RAX), var_size size, 0))], size, env1))

(* returns a list of instructions and an environment *)
let rec stmts_to_instrs ir_stmts env = 
  (* returns a list of instructions and an environment *)
  let rec stmt_to_instrs ir_stmt env = match ir_stmt with 
    | IRSVarDeclare(type_id, var_key) ->
      let size = get_var_size var_key env in 
      let (offset, env2) = push_n env size in 
      let env3 = add_var env2 var_key offset in (* push a variable to the environment stack and note where it is *)
      (* also: add instructions that also does this in the program. I also 0 initialize all variables *)
      ([BinaryOp(Sub, Register(RSP,QWord), Imm(size)); BinaryOp(Mov, MemoryAddress(get_var_length var_key env, RSP, None, 0,0), Imm(0))], env3)
    | IRSVarAssign (var_key, expression) -> 
      let length = get_var_length var_key env in 
      let (instr, _,env1) = expr_to_instrs expression env in (* value to assign will be in RAX *)
      (match get_offset env var_key with 
      | Some(offset) -> 
      (instr @ [BinaryOp(Mov, MemoryAddress(length, RSP, None, 0 , env.sp_offset - offset), Register(RAX,length))], env1) 
      | None -> (* the variable is not found on the stack, so it must be global *)
        let lbl = get_global_var_lbl env var_key in 
        (instr @ [BinaryOp(Mov, Register(RDX,QWord), Label(lbl)); BinaryOp(Mov, MemoryAddress(length, RDX, None, 0, 0), Register(RAX,length))], env1))
      
    | IRSCall(e) -> 
      let (instr, _,env1) = expr_to_instrs e env in (* calling code is generated in the expr function *)
      (instr,env1)
    | IRSDelete(var_key) -> 
      let length = get_var_length var_key env in 
      (match get_offset env var_key with 
      | Some(offset) -> 
      ([BinaryOp(Mov, Register(RDI,length), MemoryAddress(length, RSP, None, 0 , env.sp_offset - offset)) ; Call("free")], env) 
      | None -> (* the variable is not found on the stack, so it must be global *)
        let lbl = get_global_var_lbl env var_key in 
        ([BinaryOp(Mov, Register(RDX,QWord), Label(lbl)); BinaryOp(Mov, Register(RAX,length), MemoryAddress(length, RDX, None, 0, 0)); Call("free")], env))
    | IRSArrayAssign(var_id, ir_index, ir_assign) -> 
        let size = get_pointed_size var_id env in 
        let (instr1, _,env1) = expr_to_instrs ir_index env in (* compute the index, will be in RAX *)
        let (_,intermed_env) = push_int env1 in (* push index on stack *)
        let (instr2, _,env2) = expr_to_instrs ir_assign intermed_env in (* compute value, will be in RAX *) 
        let (_,final_env) = pop_int env2 in (* pop index off of stack *)
        (match get_offset final_env var_id with 
        | Some(offset) -> 
          (instr1 @ (UnaryOp(Push, Register(RAX, QWord))::instr2) @ (* Do computations, RAX=value to assign *)
          [UnaryOp(Pop, Register(RDX,QWord)) ; (* RDX = index *)
          BinaryOp(Mov, Register(RSI,QWord), MemoryAddress(QWord, RSP, None, 0, final_env.sp_offset - offset)); (* RSI=base array ptr*)
          BinaryOp(Mov, MemoryAddress(size, RSI, Some(RDX),var_size size , 0), Register(RAX,size))], final_env) (* assign performed *)
        | None -> (* the variable is not found on the stack, so it must be global *)
          let lbl = get_global_var_lbl final_env var_id in 
          (instr1 @ (UnaryOp(Push, Register(RAX, QWord))::instr2) @ (* Do computations, RAX=value to assign *)
          [UnaryOp(Pop, Register(RDX,QWord)) ; (* RDX = index *)
          BinaryOp(Mov, Register(RSI,QWord), Label(lbl)); (* RSI=base array ptr*)
          BinaryOp(Mov, MemoryAddress(size, RSI, Some(RDX),var_size size , 0), Register(RAX,size))], final_env) (* assign performed *)
        ) in

  let rec eat_stmts lst env acc = match lst with 
      | [] -> (List.rev acc, env)
      | h::tail ->
        let (instr, env2) = stmt_to_instrs h env in 
        eat_stmts tail env2 (List.rev_append instr acc) in 
  eat_stmts ir_stmts env []

(* returns a list of instructions, the type of block end, and an environment *)
(* here the stack frames are popped and RSP reset when a function ends *)
let instructionize_blockend ir_end env = match ir_end with  
    | IRSReturn(None) -> 
      let (offset, env2) = pop_frame env in 
      ([BinaryOp(Add, Register(RSP, QWord), Imm(offset - env2.sp_offset))], Ret, env)
    | IRSReturn(Some(e)) -> 
      let (instrs, _,env2) = expr_to_instrs e env in 
      let (offset, env3) = pop_frame env2 in 
      (instrs @ [BinaryOp(Add, Register(RSP, QWord), Imm(offset - env3.sp_offset))], Ret, env2)
    | IRSNoOp -> ([], NoOp, env)
    | IRSJump(bid) -> ([], Jmp(bid), env)
    | IRSBranch(e,if_b, else_b) -> 
      let (instr,size,env2) = expr_to_instrs e env in 
      (* result of expression is in RAX *)
      let branching_instructions = [BinaryOp(Cmp, Register(RAX,size), Imm(0))] in 
        (instr @ branching_instructions, Branch(Jne, if_b, else_b), env2)

(* returns a list of instructions, an environment, and a list of updated blocks *)
(* this function basically extract all IRSVarDeclares from a function in order to put them all at the beginning *)
(* this is done in order to make variables declared inside loops work properly, since otherwise they would keep being pushed to the stack *)
(* another option would be to have stack frame on variable scopes, but I opted for this *)
(* a possible optimization would be to accumulate all the stack offsets to ensure that RSP only gets modified once. something for module 3 perhaps *)
let reorder_ir_stmts ir_blocks env = 
  let rec work declare_acc other_acc = function 
  | IRSVarDeclare(t,v)::tail -> work (IRSVarDeclare(t,v)::declare_acc) other_acc tail 
  | h::tail -> work declare_acc (h::other_acc) tail 
  | [] -> (declare_acc, List.rev other_acc) in 
  let rec eat_blocks blocks declaration_acc block_acc = match blocks with 
  | [] -> (stmts_to_instrs (List.rev declaration_acc) env, List.rev block_acc )
  | (bid, (ir_stmts, ir_end))::tail -> 
    let (declarations, other_stmts) = work [] [] ir_stmts in 
    eat_blocks tail (declarations @ declaration_acc) ((bid, (other_stmts, ir_end))::block_acc)  in 
  eat_blocks ir_blocks [] []

  
(* returns a list of AsmBlocks and an environment*)
let blocks_to_asm ir_blocks env = 
  (* returns a single AsmBlock and an environment *)
  let block_to_asm (bid, (ir_stmts, ir_end)) env = 
    let (asm_instrs, env2) = stmts_to_instrs ir_stmts env in 
    let (end_instrs,asm_end, env3) = instructionize_blockend ir_end env2 in 
    (AsmBlock(bid, (asm_instrs @ end_instrs, asm_end)), env3) in 
  let rec eat_blocks blocks acc env = match blocks with 
  | [] -> (List.rev acc, env )
  | h::tail -> 
    let (asm_block, env2) = block_to_asm h env in 
    eat_blocks tail (asm_block::acc) env2 in 
  eat_blocks ir_blocks [] env

(* returns a list of AsmBlocks and an environment*)
let glob_to_asm ir_glob env = 
    (* returns a list of instructions and an environment *)
    (* ensures that the function parameters are pushed to the stack *)
    let rec push_params params_regs acc env = match params_regs with
    | [] -> (List.rev acc, env) 
    | ((p,s),r)::tail -> (
      let (offset, env2) = push_n env (var_size s) in 
      let env3 = add_var env2 p offset in
      push_params tail (BinaryOp(Mov, MemoryAddress(s, RSP, None, 0,0), Register(r,s))::BinaryOp(Sub, Register(RSP, QWord), Imm(var_size s))::acc) env3
    ) in
    (* only the IRFunc case is handled here, the others are filtered out below *)
    match ir_glob with 
    | IRFunc(id,tpe, params, blocks) -> 
        let param_sizes = find_param_sizes env id in 
        let (param_instr,env2) = push_params (shear_zip (List.combine params param_sizes) arg_registers) [] (push_frame env) in  
        let ((declarations, env3), blocks2) = reorder_ir_stmts blocks env2 in (* all variable declarations are moved to block 0 *)
        let (asm_blocks, env4) = blocks_to_asm blocks2 env3 in 
        let (_, env5) = pop_frame env4 in 
        let first_bid = (match asm_blocks with 
        | AsmBlock(bid,_)::_ -> bid (* there is always a first block *)
        ) in (* 0 is the designated block index for the parameter block *)
        (AsmFunc(id, AsmBlock(0, (param_instr @ declarations, Jmp(first_bid)))::asm_blocks), env5)

(* takes a list of CFG globals and a CFG environment and returns a list of ASM globals and an ASM environment *)
let cfg_to_asm ir_globs cfg_env = 
  (* step through the CFG globals and turn them into ASM blocks *)
  let rec convert lst acc env = match lst with 
    | [] -> (List.rev acc , env)
    | IRDecl::tail | IRVarDef::tail -> convert tail acc env 
    | h::tail -> 
      let (asm_glob, env2) = glob_to_asm h env in 
      convert tail (asm_glob::acc) env2 in 
  convert ir_globs [] { (* instantiate the ASM environment using some infor from the CFG environment *)
    sp_offset = 0 ;
    stack_frame_offsets = Frame(0, None);
    var_stack_offsets = [] ;
    var_id_to_size = cfg_env.var_key_to_size ; 
    func_param_sizes = cfg_env.function_param_sizes ; 
    string_labels = [] ; 
    next_string_label = 0 ; 
    global_vars_labels = cfg_env.global_vars_labels
  }