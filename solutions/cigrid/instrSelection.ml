open IR
open Ast
open Printf
open AsmIr
(*TODO ecall and char constants*)
let rax = Reg(0,QWord)
let rdx = Reg(2, QWord)
let r11 = Reg(11, QWord)
let r11b = Reg(11, Byte)
let funcnum = ref 0
let bitsize_of_type = function
  | _ -> QWord

let find_reg name env = 
  let (n, t) = List.assoc name env in 
  TReg((n, bitsize_of_type t),name)

let tmp_reg n = TReg((n,QWord), "tmp")

let func_register = function
  | 0 -> Reg(7,QWord)
  | 1 -> Reg(6, QWord)
  | 2 -> Reg(2,QWord)
  | 3 -> Reg(1,QWord)
  | 4 -> Reg(8, QWord)
  | 5 -> Reg(9,QWord)
  | _ -> failwith "function has too many arguments"
(*this is depending on the binary operation creating the necessarry
assembler instructions as a list. assume first operand is in r1, 
second in r2 and the result has to be stored in reg*)

let binop_select reg r1 r2 = function
  | BopAdd -> [(BinOp(Mov, reg, r1));BinOp(Add, reg, r2)]
  | BopSub -> [(BinOp(Mov, reg, r1));BinOp(Sub, reg, r2)]

  | BopMult -> [BinOp(Mov, rax, r1);
    UnOp(IMul, r2); BinOp(Mov, reg, rax)]

  | BopDiv -> [BinOp(Mov, rax, r1);
    Cqo;
    UnOp(IDiv, r2); BinOp(Mov, reg, rax)]

  | BopModulo -> [BinOp(Mov, rax, r1);
    Cqo;
    UnOp(IDiv, r2); BinOp(Mov, reg, rdx)]

    
  | BopGreater -> [BinOp(Xor, r11, r11);
    BinOp(Sub, r1, r2);
    UnOp(Setg, r11b); 
    BinOp(Mov, reg, r11)]

  | BopLesser -> [BinOp(Xor, r11, r11);
    BinOp(Sub, r1, r2);
    UnOp(Setl, r11b); 
    BinOp(Mov, reg, r11)]

  | BopGreaterEq -> [BinOp(Xor, r11, r11);
    BinOp(Sub, r1, r2);
    UnOp(Setge, r11b); 
    BinOp(Mov, reg, r11)]

  | BopLesserEq -> [BinOp(Xor, r11, r11);
    BinOp(Sub, r1, r2);
    UnOp(Setle, r11b); 
    BinOp(Mov, reg, r11)]

  | BopEqual -> [BinOp(Xor, r11, r11);
    BinOp(Sub, r1, r2);
    UnOp(Sete, r11b); 
    BinOp(Mov, reg, r11)]

  | BopNotEq -> [BinOp(Xor, r11, r11);
    BinOp(Sub, r1, r2);
    UnOp(Setne, r11b); 
    BinOp(Mov, reg, r11)]
  | _ -> failwith "otherbinopTODO"

  (*this function creates new asm expressions, and prepends them to acc
  which makes them in reverse order*)
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

  | ECall(name, exprList, ln) -> (
    match exprList with
    | e::restList -> 
      let (r1,r2) = (tmp_reg n,func_register !funcnum) in
      incr funcnum;
      let n1 = n+1 in 
      let nacc = BinOp(Mov,r2,r1)::acc in 
      let (nacc2, n2) = expr_to_asm env n1 nacc r1 e in 
      expr_to_asm env n2 nacc2 reg (ECall(name,restList,ln))
    | [] -> funcnum := 0;
      (acc@[Call(name);BinOp(Mov,reg,rax)], n)
  )
      
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

(*takes enviorement, current counter and the previous instructions
Return a blockend, extra instructions if needed and the updated counter
*)
let ir_blockend env n acc = function
  | ISReturn(eop, _) ->(
    match eop with
    | Some(e) -> let (eacc, n1) = expr_to_asm env n [] rax e in 
      ((Ret), acc@eacc,n1)
    | None -> ((Ret), acc, n)
  )
  | ISBranch(exp,s1,s2,_) -> 
    let (eacc, n1) = expr_to_asm env n [] rax exp in 
    let newop = BinOp(Cmp, rax, Imm(0))in
    (JBinOp(Jne,s1,s2), acc@eacc@[newop], n1)
  | ISJump(name,_) -> (Jmp(name), acc, n) 


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
    | Call(s)::restlist -> reg_alloc n (Call(s)::acc) restlist 
    | Cqo::restlist -> reg_alloc n (Cqo::acc) restlist 
    | [] -> acc

    (*| _ -> failwith "assert instrSelection reg_alloc"*)



let rec block_list_to_asm prevEnv n acc = function    (*create the instructions*)
  | IBlock(s,(stlist,blockend),_)::[] -> 
    let (env, n1, acc1) = (irstmt_list_to_asm prevEnv n [] stlist) in 
    let (blend, acc2, n2) = (ir_blockend env n1 acc1 blockend) in 
    let acc3 = List.rev ((BinOp(Add, Reg(4,QWord),Imm(n2*8))::(reg_alloc n2 [] acc2))) in 
    let finalblocklist = List.rev ((Block(s,(acc3,blend)))::acc) in 
    ((*extractthe first block and its accumulator to add a sub to the beginning*)
      match finalblocklist with 
      | Block(s,(ir,bend))::restlist -> 
        Block(s, (BinOp(Sub, Reg(4,QWord),Imm(n2*8))::ir,bend))::restlist
      | [] -> failwith "impossible match, ocaml broke"
    )  

  | IBlock(s,(stlist,blockend),_)::restlist -> 
    let (env, n1, acc1) = (irstmt_list_to_asm prevEnv n [] stlist) in 
    (*handle the blockend*) 
    let (blend, acc2, n2) = (ir_blockend env n1 acc1 blockend) in 
    (*this is supposed to spill just one block , not push pop anything*)
    let acc3 = List.rev (reg_alloc n2 [] acc2) in 
    block_list_to_asm env n2 ((Block(s,(acc3,blend)))::acc) restlist

  | [] -> failwith "no blocks to compile"

    


let ir_global_to_asm = function
    | IFunc(s, (t, listTySt, blist),_) -> Func(s,(block_list_to_asm [] 0 [] blist))




    (*okay, this is trying to sometimes access variables that it shouldnt, just because it 
    can slip through when you dont type check, that a variable is being used withou declaration
    supress for now*)