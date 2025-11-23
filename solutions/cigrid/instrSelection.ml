open IR
open Ast
open Printf
let bitsize_of_type = function
  | _ -> AsmIr.QWord

let find_reg name env = 
  let (n, t) = List.assoc name env in 
  AsmIr.TReg((n, bitsize_of_type t),name)

  let tmp_reg n = AsmIr.TReg((n,AsmIr.QWord), "tmp")

let rec expr_to_asm env n acc reg = function
  | EVar(name, _) -> ((AsmIr.BinOp(AsmIr.Mov, reg, find_reg name env))::acc, n)
  | EInt(n, _) -> ((AsmIr.BinOp(AsmIr.Mov, reg, AsmIr.Imm(n)))::acc,n)
  | EChar(c,_) -> ((AsmIr.BinOp(AsmIr.Mov, reg, AsmIr.Imm(int_of_char c)))::acc,n)
  | EString(s, _) -> printf "stringTODO"; exit 4
  | EBinOp(BopAdd,e1,e2,_) -> 
    let (r1, r2) = (tmp_reg n, tmp_reg(n+1)) in
    let n1 = n+2 in 
    let acc1 = (AsmIr.BinOp(AsmIr.Mov, reg, r1))::(AsmIr.BinOp(AsmIr.Add, reg, r2))::acc in 
    let (acc2, n2) = expr_to_asm env n1 acc1 r2 e2 in 
    expr_to_asm env n2 acc2 r1 e1
  | EBinOp(BopSub,e1,e2,_) -> 
    let (r1, r2) = (tmp_reg n, tmp_reg(n+1)) in
    let n1 = n+2 in 
    let acc1 = (AsmIr.BinOp(AsmIr.Mov, reg, r1))::(AsmIr.BinOp(AsmIr.Sub, reg, r2))::acc in 
    let (acc2, n2) = expr_to_asm env n1 acc1 r2 e2 in 
    expr_to_asm env n2 acc2 r1 e1
  | EBinOp(uop,e1,e2,_) -> printf "otherbinopTODO"; exit 4
  | EUnOp(UnOpMinus, exp,_) -> let acc1 = (AsmIr.UnOp(AsmIr.Neg, reg))::acc in
    expr_to_asm env n acc1 reg exp 
  | EUnOp(uop, exp,_) -> printf "otherunopTODO"; exit 4
  | ECall(name, exprList, _) -> printf "ecallTODO"; exit 4
  | ENew(t,exp,_) -> printf "enewTODO"; exit 4
  | EArrayAccess(name,elementNum,structfieldopt,_)-> printf "earrayaccessTODO"; exit 4

let rec irstmt_list_to_asm env n acc = function
  | ISVarDecl(name, t, _)::restlist -> let nenv = (name,(n,t))::env in
    irstmt_list_to_asm nenv (n+1) acc restlist
  | ISVarAssign(name, exp, _)::restlist ->  
    let (eacc, n2) = expr_to_asm env n [] (find_reg name env) exp in
    irstmt_list_to_asm env n2 (List.rev_append eacc acc) restlist
  | ISExpr(e,_)::restlist -> printf "ISExprTODO"; exit 4
  | [] -> (env, n, List.rev acc)


let ir_blockend env n acc = function
  | ISReturn(eop, _) ->(
    match eop with
    | Some(e) -> let (eacc, n1) = expr_to_asm env n [] (AsmIr.Reg(0, AsmIr.QWord)) e in 
      ((AsmIr.Ret), acc@eacc,n1)
    | None -> ((AsmIr.Ret), acc, n)
  )
  | ISBranch(exp,n1,n2,_) -> printf "ISExprTODO"; exit 4
  | ISJump(name,_) -> printf "ISExprTODO"; exit 4

let rec block_list_to_asm n acc = function
  | IBlock(s,(stlist,blockend),_)::restlist -> let (env, n1, acc1) = (irstmt_list_to_asm [] n acc stlist) in 
    let (blend, acc2, n2) = (ir_blockend env n1 acc1 blockend) in
    (AsmIr.Block("main",(acc2,blend)))::(block_list_to_asm n2 [] restlist)
  | [] -> []
let ir_global_to_asm = function
    | IFunc(s, (t, listTySt, blist),_) -> AsmIr.Func("main",(block_list_to_asm 0 [] blist))

