open Printf

type unop = Inc | Dec | Push | Pop | IMul | IDiv | Not | Neg | Setg | Setl |
Setge | Setle | Sete | Setne

type binop = Add | Sub | Cmp | Mov | And | Or | Xor

type bitsize =
| Byte | Word | DWord | QWord

type reg = int * bitsize

type op = 
  | Imm of int 
  | Reg of reg
  | TReg of reg * string
  (*| Mem of bitsize * reg * reg option * scale * displacement*)
  | Mem of bitsize * reg * reg option * int * int
  | NoOp

type jbinop = Jl | Jg | Jle | Jge | Je | Jne

type blockend =
  | Ret
  | Jmp of string
  | JBinOp of jbinop * string * string

type inst =
| UnOp of unop * op
| BinOp of binop * op * op
| Call of string
| Cqo

type block = Block of string * (inst list * blockend)

type func = Func of string * block list

let pprint_jbinop = function
  | Jl -> "jl"
  | Jg -> "jg"
  | Jle -> "jle"
  | Jge -> "jge"
  | Je -> "je"
  | Jne-> "jne"
let pprint_block_end = function
  | Ret -> "\tret\n"
  | Jmp(s) -> "\tjmp\t" ^ s ^ "\n"
  | JBinOp(jb, s1,s2) -> "\t" ^ (pprint_jbinop jb) ^ "\t"^ s1 ^ 
    "\n\tjmp\t"^ s2 ^ "\n"


let pprint_uop = function 
  | Inc -> "inc" 
  | Dec -> "dec" 
  | Push -> "push" 
  | Pop -> "pop"
  | IMul -> "imul" 
  | IDiv -> "idiv" 
  | Not -> "not" 
  | Neg -> "neg" 
  | Setg -> "setg" 
  | Setl -> "setl" 
  | Setge -> "setge" 
  | Setle -> "setle" 
  | Sete -> "sete" 
  | Setne -> "setne" 

let pprint_bop = function
  | Add -> "add" 
  | Sub -> "sub" 
  | Cmp -> "cmp" 
  | Mov -> "mov" 
  | And -> "and" 
  | Or -> "or" 
  | Xor -> "xor" 

let pprint_bitsize = function
  | Byte -> "byte" 
  | Word -> "word" 
  | DWord -> "dword" 
  | QWord -> "qword" 

let pprint_reg = function
  | (0,_) -> "rax"
  | (1,_) -> "rcx"
  | (2,_) -> "rdx"
  | (3,_) -> "rbx"
  | (4,_) -> "rsp"
  | (5,_) -> "rbp"
  | (6,_) -> "rsi"
  | (7,_) -> "rdi"
  | (8,_) -> "r8"
  | (9,_) -> "r9"
  | (10,_) -> "r10"
  | (11,_) -> "r11"
  | (12,_) -> "r12"
  | (13,_) -> "r13"
  | (14,_) -> "r14"
  | (15,_) -> "r15"
  | _ -> failwith "not a real register, wtf"


let pprint_op = function 
  | Imm(n) -> sprintf "%d" n
  | Reg(n,b) -> pprint_reg (n,b)
  | TReg((n,bits), s) -> sprintf "%s_%d" s n
  | Mem(bit,r1,ropt,scale,disp) ->(
    match ropt with
    | Some(r2) -> sprintf "[%s + %s * %d + %d]" (pprint_reg r1) (pprint_reg r2) scale disp
    | None -> sprintf "qword [%s + %d]" (pprint_reg r1) disp
  ) 
  | NoOp -> ""

let pprint_instruction = function
  | UnOp(uop, op1) -> "\t" ^ (pprint_uop uop) ^ "\t" ^ (pprint_op op1) ^ "\n"
  | BinOp(bop, op1, op2) -> "\t" ^ (pprint_bop bop) ^ "\t" ^ (pprint_op op1) ^ ", " ^ (pprint_op op2) ^ "\n"
  | Call(s)  -> "\tcall\t" ^ s ^"\n"
  | Cqo -> "\tcqo\n"

let pprint_instruction_list instList =
  String.concat "" (List.map pprint_instruction instList)

 let pprint_block = function 
  | Block(s,(instList, blEnd)) -> s^":\n" ^ (pprint_instruction_list instList) ^ (pprint_block_end blEnd)

let rec pprint_block_list acc = function
  | bl::restlist -> let nacc = acc^(pprint_block bl) in
   pprint_block_list nacc restlist
  | [] -> acc

let pprint_func = function
  | Func(s, bList) -> (pprint_block_list "" bList)