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
  | Jl -> "Jl"
  | Jg -> "Jg"
  | Jle -> "Jle"
  | Jge -> "Jge"
  | Je -> "Je"
  | Jne-> "Jne"

let pprint_block_end = function
  | Ret -> "\tret\n"
  | Jmp(s) -> "\tjmp\t" ^ s ^ "\n"
  | JBinOp(jb, s1,s2) -> "\t" ^ (pprint_jbinop jb) ^ s1 ^ ", " ^ s2 ^ "\n"


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
  | 0 -> "rax"
  | 1 -> "rcx"
  | 2 -> "rdx"
  | 3 -> "rbx"
  | 4 -> "rsp"
  | 5 -> "rbp"
  | 6 -> "rsi"
  | 7 -> "rdi"
  | 8 -> "r8"
  | 9 -> "r9"
  | 10 -> "r10"
  | 11 -> "r11"
  | 12 -> "r12"
  | 13 -> "r13"
  | 14 -> "r14"
  | 15 -> "r15"
  | _ -> Printf.printf "not a real register, wtf"; exit 4


let pprint_op = function 
  | Imm(n) -> sprintf "%d" n
  | Reg(n,_) -> pprint_reg n
  | TReg((n,bits), s) -> s
  | Mem(bit,r1,ropt,scale,disp) -> "akdsjhak"
  | NoOp -> ""

let pprint_instruction = function
  | UnOp(uop, op1) -> "\t" ^ (pprint_uop uop) ^ "\t" ^ (pprint_op) ^ "\n"
  | BinOp(bop, op1, op2) -> "\t" ^ (pprint_bop bop) ^ "\t" ^ (pprint_op) ^ ", " ^ (pprint_op) ^ "\n"
  | Call(s)  -> Printf.printf "TODO"; exit 4
  | Cqo ->  -> Printf.printf "TODO"; exit 4

let pprint_instruction_list instList =
  List.concat_map pprint_instruction instList 

 let pprint_block = function 
  | Block(s,(instList, blEnd)) -> (pprint_instruction_list instList) ^ (pprint_block_end blEnd)

let pprint_block_list = function
  | [bl] -> pprint_block bl
  | _ -> Printf.printf "shouldnt be more than one block"; exit 4

let pprint_func = function
  | Func(s, bList) -> "main:\n"^(pprint_block_list bList)