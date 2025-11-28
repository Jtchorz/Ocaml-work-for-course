open IR
open Ast
open Printf
open AsmIr
(*TODO ecall and char constants*)
let rax = Reg(0,QWord)
let rcx = Reg(1, QWord)
let cl = Reg(1, Byte)
let rdx = Reg(2, QWord)
let rdi = Reg(7, QWord)
let r10 = Reg(10, QWord)
let r11 = Reg(11, QWord)
let r12 = Reg(12, QWord)
let r11b = Reg(11, Byte)
let funcnum = ref 0
let gldeclnum = ref 0
let buf = Buffer.create 16
let bitsize_of_type = function
  | _ -> QWord

  (*write something here that will try to find in env and if it cant
    it will write an access to the global as a return, this will have to add a new
  operand, which will have to be spilled differently*)
  (*if something cannot find the thing in env, it is going to just call it from
  the fckn data section*)
let find_reg name env = 
  match List.assoc_opt name env with 
  | Some(n,t) -> TReg((n, bitsize_of_type t),name)
  | None -> GVar(name)

let tmp_reg n = TReg((n,QWord), "tmp")

let func_register = function
  | 0 -> Reg(7,QWord)
  | 1 -> Reg(6, QWord)
  | 2 -> Reg(2,QWord)
  | 3 -> Reg(1,QWord)
  | 4 -> Reg(8, QWord)
  | 5 -> Reg(9,QWord)
  | _ -> failwith "function has too many arguments"

(*this gets them in the right order to make them easy to write*)
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

  | BopBitAnd -> [BinOp(And, r1, r2);
    BinOp(Mov, reg, r1)]

  | BopBitOr -> [BinOp(Or, r1, r2);
    BinOp(Mov, reg, r1)]

  | BopAnd -> [ BinOp(Xor, r11, r11);
    BinOp(Sub, r1, Imm(0));
    UnOp(Setne,r11b);
    BinOp(Mov, r1, r11);
    BinOp(Xor, r11, r11);
    BinOp(Sub, r2, Imm(0)); 
    UnOp(Setne,r11b); 
    BinOp(Mov, r2, r11);(*normalise them to 0 or 1*) 
    (*bitwise and will do the trick now*)
    BinOp(And, r1, r2);
    BinOp(Mov, reg, r1)
  ]
  (*if any of them have any bits set then the result should be 1*)
  | BopOr -> [BinOp(Xor, r11, r11);
    BinOp(Or, r1, r2); (*this will set ZF to 1 if the result is 0 [both are zero] and 
      setne == setnz*)
    UnOp(Setne, r11b);
    BinOp(Mov, reg, r11)]

  | BopShiftLeft -> [BinOp(Mov, rcx, r2);
    BinOp(Shl, r1, cl);
    BinOp(Mov, reg, r1)]

  | BopShiftRight -> [BinOp(Mov, rcx, r2);
    BinOp(Shr, r1, cl);
    BinOp(Mov, reg, r1)]

let unop_select reg = function
 | UnOpMinus -> [(UnOp(Neg, reg))]
 | UnOpBitFlip -> [(UnOp(Not, reg))]
 | UnOpNegation -> [BinOp(Xor, r11, r11);
    BinOp(Sub, reg, Imm(0));
    UnOp(Sete, r11b); 
    BinOp(Mov, reg, r11)]


let string_to_asm s =
  let b = Buffer.create 20 in 
  String.iter( fun c ->
    Buffer.add_string b (match c with
      | '\n' -> " 10," 
      | '\t' -> " 9," 
      | '\\' -> " 92,"
      |  '\''-> " 39,"
      |  '\"' -> " 34,"
      | c -> " \""^(String.make 1 c)^"\","
  ) ) s;
   (Buffer.contents b) ^ " 0"


    
(*this function has them in the wrong order for efficient adding*)
let rec expr_to_asm env n acc reg = function
  | EVar(name, _) -> ((BinOp(Mov, reg, find_reg name env))::acc, n)
  | EInt(nu, _) -> ((BinOp(Mov, reg, Imm(nu)))::acc,n)
  | EChar(c,_) -> ((BinOp(Mov, reg, Imm(int_of_char c)))::acc,n)
  
  | EString (s,_) -> let sname = "string"^(string_of_int !gldeclnum) in
    incr gldeclnum; 
    Buffer.add_string buf ("\t"^sname^":\tdb "^(string_to_asm s)^"\n");
    ((BinOp(Mov, reg, GString(sname)))::acc,n) 

  | EBinOp(bop,e1,e2,_) -> (
    let (r1, r2) = (tmp_reg n, tmp_reg(n+1)) in
    let n1 = n+2 in 
    let acc1 = ((binop_select reg r1 r2 bop))@acc in 
    let (acc2, n2) = expr_to_asm env n1 acc1 r2 e2 in 
    expr_to_asm env n2 acc2 r1 e1 
  )

  | EUnOp(uop, exp,_) -> let acc1 = (unop_select reg uop)@acc in
    expr_to_asm env n acc1 reg exp 

  | ECall(name, exprList, ln) -> 
    let rec work acc n = function
    | e::restlist -> 
      let (r1,r2) = (tmp_reg n,func_register !funcnum) in
      incr funcnum;
      let n1 = n + 1 in
      let nacc = BinOp(Mov,r2,r1)::acc in 
      let (nacc2, n2) = expr_to_asm env n1 nacc r1 e in 
      work nacc2 n2 restlist
    | [] -> 
      funcnum := 0; (acc,n)
    in 
    let (nacc, n1) = 
    if name = "printf" then 
      (work [BinOp(Xor,rax,rax);Call(name);BinOp(Mov,reg,rax)] n exprList)
    else
     (work [Call(name);BinOp(Mov,reg,rax)] n exprList) in 
    (nacc@acc, n1)

  | ENew(t,exp,_) -> (
    let r1 = tmp_reg n in 
    let n1 = n+1 in 
    let acc1 = [BinOp(Mov,rdi, r1);BinOp(Shl, rdi, Imm(8));Call("malloc");BinOp(Mov,reg,rax)]@acc in 
    expr_to_asm env n1 acc1 r1 exp 
  )
  | EArrayAccess(name,elementNum,structfieldopt,_)-> (
    match structfieldopt with 
    | Some(_) -> failwith "not implementing structs"
    | None -> 
      let (r1, rA) = (tmp_reg n, find_reg name env) in
      let n1 = n+1 in 
      let acc1 = [
        BinOp(Mov, r10, rA);
        BinOp(Mov, r11, r1);
        BinOp(Mov, reg, Mem(QWord, (10,QWord), Some((11,QWord)), 8,0))]@acc in 
      expr_to_asm env n1 acc1 r1 elementNum
  )

let rec irstmt_list_to_asm env n acc = function
  | ISVarDecl(name, t, _)::restlist -> let nenv = (name,(n,t))::env in
    irstmt_list_to_asm nenv (n+1) acc restlist 

  | ISVarAssign(name, exp, _)::restlist ->  
    let (eacc, n2) = expr_to_asm env n [] (find_reg name env) exp in
    irstmt_list_to_asm env n2 (List.rev_append eacc acc) restlist 
(*add a ISGStringAssign*)
  | ISExpr(e,_)::restlist -> 
    let (eacc, n2) = expr_to_asm env n [] rax e in 
    irstmt_list_to_asm env n2 (List.rev_append eacc acc) restlist

  | ISDelete(s,_)::restlist -> 
    let r1 = find_reg s env in 
    let eacc = [Call("free"); BinOp(Mov,rdi,r1)]@acc in
    irstmt_list_to_asm env n eacc restlist 

  | ISArrayAssign(name,index,e,_)::restlist ->
    let (tInd,tE, rA) = (tmp_reg n, tmp_reg (n+1), find_reg name env) in 
    let n1 = n+2 in   
    let (eacc1, n2 ) = expr_to_asm env n1 [] tE e in
    let (eacc2, n3) = expr_to_asm env n2 eacc1 tInd index in (*we have to eval exp first bcus index can x++*)
    (*eacc2 is in the right order, to add to its end we reverse it, and we store it like that*)
    let eacc3 = 
      [BinOp(Mov, Mem(QWord,(10, QWord), Some(11,QWord), 8,0), tE); 
      BinOp(Mov, r10, rA);
      BinOp(Mov, r11, tInd)]@(List.rev eacc2) in
    irstmt_list_to_asm env n3 (eacc3@acc) restlist

  | [] -> (env, n, List.rev acc)

(*takes enviorement, current counter and the previous instructions
Return a blockend, extra instructions if needed and the updated counter
jmps to a secret thing to prevent very bad behavior sometimes
  *)
let ir_blockend env n acc = function
  | ISReturn(eop, _) ->(
    match eop with 
    | Some(e) -> let (eacc, n1) = expr_to_asm env n [] rax e in 
      ((Jmp("________"^(string_of_int(!gldeclnum)))), acc@eacc,n1)
    | None -> ((Jmp("________"^(string_of_int(!gldeclnum)))), acc, n)
  )
  | ISBranch(exp,s1,s2,_) -> 
    let (eacc, n1) = expr_to_asm env n [] rax exp in 
    let newop = BinOp(Cmp, rax, Imm(0))in
    (JBinOp(Jne,s1,s2), acc@eacc@[newop], n1)
  | ISJump(name,_) -> (Jmp(name), acc, n) 


  (*this returns in reverse order*)
let bop_spill bop acc op1 op2 =
  match op2 with
  | Imm(_) | Reg(_)  -> ( 
    match op1 with 
    | TReg((n,b),_) -> (BinOp(bop, Mem(b,(4,QWord),None,0,(n*8)), op2))::acc
    | Reg(_) | GVar(_) -> (BinOp(bop, op1, op2))::acc
    | GString(_) -> failwith "cant write to strings"
    | Mem(_) -> failwith "tried spilling from mem to mem, notthought of"
    | Imm(_) | NoOp -> failwith "impossible"
    )

  | Mem(_) | GString(_) | GVar(_) -> (
    match op1 with  
    | Reg(_) -> 
      (BinOp(bop, op1, op2))::acc
    | Mem(_) | GVar(_) -> [(BinOp(bop, op1, r12));
      BinOp(Mov, r12, op2)]@acc
    | TReg((n1,b1),_) -> 
      [BinOp(bop, Mem(b1,(4,QWord),None,0,(n1*8)),r12);
      BinOp(Mov, r12, op2)]@acc
    | GString(_) -> failwith "cant write to strings"
    | Imm(_) | NoOp -> failwith "impossible"
    )

    
  | TReg((n,b),_) -> (
    match op1 with  
    | Reg(_) -> 
      (BinOp(bop, op1, Mem(b,(4,QWord),None,0,(n*8))))::acc
    | Mem(_) | GVar(_) -> [(BinOp(bop, op1, r12));
      BinOp(Mov, r12, Mem(b,(4,QWord),None,0,(n*8)))]@acc
    | TReg((n1,b1),_) -> 
      (BinOp(bop, Mem(b1,(4,QWord),None,0,(n1*8)),r10))::(BinOp(Mov, r10, Mem(b,(4,QWord),None,0,(n*8))))::acc
    | GString(_) -> failwith "cant write to strings"
    | Imm(_) | NoOp -> failwith "impossible"
    )
  
  | NoOp -> failwith "impossible"

let uop_spill acc uop op =
  match op with
  | Imm(_) | Reg(_) | GString(_) | GVar(_) -> (UnOp(uop, op))::acc
  | TReg((n,b),_) -> (UnOp(uop, (Mem(b,(4,QWord),None,0,(n*8))) ))::acc
  | Mem(_) | NoOp -> failwith "impossible" 

(*these will return in rev order because above returns that way*)
let rec reg_alloc n acc = function
    | BinOp(bop, op1, op2)::restlist -> let acc2 = bop_spill bop acc op1 op2 in 
      reg_alloc n acc2 restlist 
    | UnOp(uop, op)::restlist -> let acc2 = uop_spill acc uop op in 
      reg_alloc n acc2 restlist 
    | Call(s)::restlist -> reg_alloc n (Call(s)::acc) restlist 
    | Cqo::restlist -> reg_alloc n (Cqo::acc) restlist 
    | [] -> acc

    (*| _ -> failwith "assert instrSelection reg_alloc"*)


(*this implementation is extra faulty, it makes returns inside of everything
not sub the stack correctly maybe in the irgen instead of
returning, create a secret block at the end? one that you always jump to, and add it in here?
*)
let rec block_list_to_asm prevEnv n acc = function    (*create the instructions*)
  | IBlock(s,(stlist,blockend),_)::[] -> 
    let (env, n1, acc1) = (irstmt_list_to_asm prevEnv n [] stlist) in 
    let (blend, acc2, n2) = (ir_blockend env n1 acc1 blockend) in 
    let acc3 = List.rev (reg_alloc n2 [] acc2) in
    let finalblocklist = 
      List.rev (
        [Block("________"^(string_of_int(!gldeclnum)),([UnOp(Pop, r12);BinOp(Add, Reg(4,QWord),Imm(n2*8))], Ret));
        Block(s,(acc3,blend))]@acc) in 
    incr gldeclnum;
    ((*just add a special block of name "________" that will sub and ret, assume*)
      match finalblocklist with 
      | Block(s,(ir,bend))::restlist -> 
        (Block(s, (BinOp(Sub, Reg(4,QWord),Imm(n2*8))::ir,bend))::restlist)
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


(*this should return a block with the name of the function, and the name of the next block
which should be like _funcname_ also return the n, and the enviorement*)
let handle_args name listTyStr = 
  let rec work env n acc = function
    | (t,name)::restlist -> 
      let nenv = (name,(n,t))::env in
      let (r1,r2) = (find_reg name nenv,func_register !funcnum) in
      incr funcnum;
      let n1 = n + 1 in
      let nacc = BinOp(Mov,r1,r2)::acc in 
      work nenv n1 nacc restlist
    | [] -> 
      funcnum := 0; (env, n, List.rev acc)
  in let (nenv, n1, acc) = (work [] 0 [UnOp(Push, r12)] listTyStr) in 
  let nacc = List.rev (reg_alloc n1 [] acc) in (*spill them*)
  (nenv, n1, Block(name,(nacc,Jmp("_"^name^"_"))))

  (*the first block and env is created to handle input arguments 
  *)
let ir_global_to_asm iList =
    let rec work acc = function
    | IFunc(s, (t, listTySt, blist),_)::restlist -> 
      let (env, n, block) = handle_args s listTySt in
      let nacc = Func(s,(block_list_to_asm env n [block] blist))::acc in 
      work nacc restlist 
    | IGVarDef(t,s,e,_)::restlist ->(
      match t with 
      | TInt ->(
        match e with 
        | EInt(num, _) ->
         Buffer.add_string buf ("\t"^s^":\tdq\t"^(string_of_int num)^"\n"); 
         work acc restlist
        | _ -> failwith "nuh uh"
      )
      | _ -> failwith "nuh uh"
      ) 
    | [] -> (Buffer.contents buf, List.rev acc)
    in Buffer.add_string buf "\tsection .data\n";
    work [] iList
