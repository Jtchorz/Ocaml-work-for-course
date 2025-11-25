open IR
open Ast
open Printf
open AsmIr

let rax = Reg(0,QWord)
let bitsize_of_type = function
  | _ -> QWord

let find_reg name env = 
  let (n, t) = List.assoc name env in 
  TReg((n, bitsize_of_type t),name)

  let tmp_reg n = TReg((n,QWord), "tmp")

(*this is depending on the binary operation creating the necessarry
assembler instructions as a list. assume first operand is in r1, 
second in r2 and the result has to be stored in reg*)

let binop_select reg r1 r2 = function
  | BopAdd -> [(BinOp(Mov, reg, r1));BinOp(Add, reg, r2)]
  | BopSub -> [(BinOp(Mov, reg, r1));BinOp(Sub, reg, r2)]
  | BopMult -> [BinOp(Mov, rax, r1);
    UnOp(IMul, r2); BinOp(Mov, reg, rax)]
  | _ -> failwith "otherbinopTODO"

let rec expr_to_asm env n acc reg = function
  | EVar(name, _) -> ((BinOp(Mov, reg, find_reg name env))::acc, n)
  | EInt(nu, _) -> ((BinOp(Mov, reg, Imm(nu)))::acc,n)
  | EChar(c,_) -> ((BinOp(Mov, reg, Imm(int_of_char c)))::acc,n)
  | EString(s, _) -> failwith "stringTODO"
  | EBinOp(bop,e1,e2,_) -> (
    let (r1, r2) = (tmp_reg n, tmp_reg(n+1)) in
    let n1 = n+2 in 
    let acc1 = (binop_select reg r1 r2 bop)@acc in 
    let (acc2, n2) = expr_to_asm env n1 acc1 r2 e2 in 
    expr_to_asm env n2 acc2 r1 e1 
  )
  | EUnOp(UnOpMinus, exp,_) -> let acc1 = (UnOp(Neg, reg))::acc in
    expr_to_asm env n acc1 reg exp 
  | EUnOp(uop, exp,_) -> failwith "otherunopTODO"
  | ECall(name, exprList, _) -> failwith "ecallTODO"
  | ENew(t,exp,_) -> failwith "enewTODO"
  | EArrayAccess(name,elementNum,structfieldopt,_)-> failwith "earrayaccessTODO"

let rec irstmt_list_to_asm env n acc = function
  | ISVarDecl(name, t, _)::restlist -> let nenv = (name,(n,t))::env in
    irstmt_list_to_asm nenv (n+1) acc restlist
  | ISVarAssign(name, exp, _)::restlist ->  
    let (eacc, n2) = expr_to_asm env n [] (find_reg name env) exp in
    irstmt_list_to_asm env n2 (List.rev_append eacc acc) restlist
  | ISExpr(e,_)::restlist -> failwith "ISExprTODO"
  | [] -> (env, n, List.rev acc)


let ir_blockend env n acc = function
  | ISReturn(eop, _) ->(
    match eop with
    | Some(e) -> let (eacc, n1) = expr_to_asm env n [] (Reg(0, QWord)) e in 
      ((Ret), acc@eacc,n1)
    | None -> ((Ret), acc, n)
  )
  | ISBranch(exp,n1,n2,_) -> failwith "IBrancTODO"
  | ISJump(name,_) -> failwith "ISJumpTODO" 


(*the memory access in this function has to be fixed, it is haphazard and incorrect propably*)
let bop_spill bop acc op1 op2 =
    match op2 with
    | Imm(_) | Reg(_) -> ( 
      match op1 with 
      | TReg((n,b),_) -> (BinOp(bop, Mem(b,(4,QWord),None,0,(n*8)), op2))::acc
      | Reg(_) | Mem(_) -> (BinOp(bop, op1, op2))::acc
      | Imm(_) | NoOp -> failwith "impossible"
      )
    | TReg((n,b),_) -> (
      match op1 with  
      | Reg(_) -> 
        (BinOp(bop, op1, Mem(b,(4,QWord),None,0,(n*8))))::acc
      | TReg((n1,b1),_) -> 
        (BinOp(bop, Mem(b1,(4,QWord),None,0,(n1*8)),Reg(10,QWord)))::(BinOp(Mov, Reg(10,QWord), Mem(b,(4,QWord),None,0,(n*8))))::acc
      | Mem(_) -> failwith "TODOMem"
      | Imm(_) | NoOp -> failwith "impossible"
      )
    | Mem(_) | NoOp -> failwith "impossible"

    let uop_spill acc uop op =
      match op with
      | Imm(_) | Reg(_) -> (UnOp(uop, op))::acc
      | TReg((n,b),_) -> (UnOp(uop, (Mem(b,(4,QWord),None,0,(n*8))) ))::acc
      | Mem(_) | NoOp -> failwith "impossible"

let rec reg_alloc n acc = function
    | BinOp(bop, op1, op2)::restlist -> let acc2 = bop_spill bop acc op1 op2 in 
      reg_alloc n acc2 restlist 
    | UnOp(uop, op)::restlist -> let acc2 = uop_spill acc uop op in 
      reg_alloc n acc2 restlist 
    | [] -> List.rev ((BinOp(Add, Reg(4,QWord),Imm(n*8)))::acc)
    | _ -> failwith "only doing bop for now"



let rec block_list_to_asm n acc = function    (*create the instructions*)
  | IBlock(s,(stlist,blockend),_)::restlist -> let (env, n1, acc1) = (irstmt_list_to_asm [] n [] stlist) in 
    (*handle the blockend*)
    let (blend, acc2, n2) = (ir_blockend env n1 acc1 blockend) in
    (*spill the registers here because it is convienient*)
    let acc3 = reg_alloc n2 [BinOp(Sub, Reg(4,QWord),Imm(n2*8))] acc2 in
    (*return the whole assembled block ACC/ACC3 here *)
    block_list_to_asm n2 ((Block("main",(acc3,blend)))::acc) restlist
  | [] -> List.rev acc 


let ir_global_to_asm = function
    | IFunc(s, (t, listTySt, blist),_) -> Func("main",(block_list_to_asm 0 [] blist))




    (*okay, this is trying to sometimes access variables that it shouldnt, just because it 
    can slip through when you dont type check, that a variable is being used withou declaration
    supress for now*)