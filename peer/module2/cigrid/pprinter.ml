open Parser
open Printf
open Lexer
open Ast
open Cfg
open Asm

(* -------------------------- pprint tokens for debug purposes ---------------------------*)
let print_token tok = match tok with
  |LParen(_) -> "LPAREN"
  |RParen(_) -> "RPAREN"
  | LBracket(_) -> "LBRACK"
  | RBracket(_) -> "RBRACK"
  | LCurly(_) -> "LCURLY"
  | RCurly(_) -> "RCURLY"
  | Comma(_) -> "COMMA"
  | Dot(_) -> "DOT"
  | SemiC(_) -> ";"
  | Not(_) -> "!"
  | BinaryNot(_) -> "~"
  | Plus(_) -> "+"
  | Minus(_) -> "-"
  | Multiplication(_) -> "*"
  | Division(_) -> "/"
  | Modulo(_) -> "%"
  | Inc(_) -> "++"
  | Dec(_) -> "--"
  | LT(_) -> "<"
  | GT(_) -> ">"
  | LEQ(_) -> "<="
  | GEQ(_) -> ">="
  | Equality(_) -> "=="
  | Inequality(_) -> "!="
  | BinaryAnd(_) -> "&"
  | BinaryOr(_) -> "|"
  | LogicalAnd(_) -> "&&"
  | LogicalOr(_) -> "||"
  | ShiftLeft(_) -> "<<"
  | ShiftRight(_) -> ">>"
  | Assign(_) -> "="
  | Void(_) -> "void"
  | TypeInt(_) -> "int"
  | TypeChar(_) -> "char"
  | TypeStruct(_) -> "struct"
  | For(_) -> "for"
  | While(_) -> "while"
  | If(_) -> "if"
  | Else(_) -> "else"
  | Break(_) -> "break"
  | Return(_) -> "return"
  | Delete(_) -> "delete"
  | Extern(_) -> "extern"
  | New(_) -> "new"
  |Identifier(_,s) -> sprintf "IDENT(%s)" s
  | IntConstant(_,n) -> string_of_int n
  | CharConstant(_,s) -> s
  | StringConstant(_,s) -> s
  |EOF(_)-> "EOF"
  |ErrorLex(_,s) -> "ERROR: " ^ s

let string_of_tokens toks = 
  let rec work toks acc = match toks with 
  |[] -> String.concat "  " (List.rev acc)
  | h::tail -> work tail ((print_token h)::acc)
in work toks []

let get_token_list lexbuf =
  let rec work acc =
    match Lexer.token_rule lexbuf with
    | EOF(x) -> (List.rev (EOF(x)::acc))
    | t -> work (t::acc)
  in work []

(* ------------------------ pprint AST ----------------------------- *)
let pprint_identifier tree = sprintf "\"%s\"" (snd tree)

let rec pprint_type tree = 
  let (_, t) = tree in 
  match t with
  | TVoid -> "TVoid"
  | TInt -> "TInt"
  | TChar -> "TChar"
  | Identifier(identifier) -> sprintf "TIdent(%s)" (pprint_identifier identifier)
  | TPoint(type_id) -> sprintf "TPoint(%s)" (pprint_type type_id)

let rec pprint_parameters tree = 
  let rec work params acc = match params with 
  | [] -> String.concat " " (List.rev acc)
  | (_,Param(type_id, identifier))::tail -> work tail ((sprintf "(%s, %s)" (pprint_type type_id) (pprint_identifier identifier))::acc) in
  sprintf "{%s}" (work tree [])
 
let pprint_fields tree = 
  let rec work fields acc = match fields with 
  | [] -> String.concat "\n" (List.rev acc)
  | (_,Field(type_id, identifier))::tail -> work tail ((sprintf "(%s, %s)" (pprint_type type_id) (pprint_identifier identifier))::acc) in
  sprintf "{\n%s\n}" (work tree [])

let pprint_unary tree = 
  let (_, t) = tree in 
  match t with 
  |LogNotOp -> "!"
  | BinNotOp -> "~" 
  | UnaryMinOp -> "-" 

let pprint_binary tree = 
  let (_, t) = tree in 
  match t with 
  | PlusOp -> "+"
  | MinusOp -> "-" 
  | MultOp -> "*" 
  | DivOp -> "/" 
  | ModOp -> "%" 
  | LTOp -> "<" 
  | GTOp -> ">" 
  | LEQOp -> "<=" 
  | GEQOp -> ">="
  | EqOp -> "=="
  | NeqOp -> "!=" 
  | BinAndOp -> "&" 
  | BinOrOp -> "|"
  | LogAndOp -> "&&" 
  | LogOrOp -> "||" 
  | ShiftLeftOp -> "<<"
  | ShiftRightOp -> ">>" 

let rec pprint_expression tree = 
  let (_, t) = tree in 
  match t with 
  | EVar(identifier) -> sprintf "EVar(%s)" (pprint_identifier identifier) 
  | EInt ( n ) -> sprintf "EInt(%d)" n
  | EChar (c ) -> sprintf "EChar(\'%s\')" c 
  | EString (s ) -> sprintf "EString(\"%s\")" s
  | EUnOp (unary_op,expression ) -> sprintf "EUnOp(%s, %s)" (pprint_unary unary_op) (pprint_expression expression)
  | EBinOp (binary_op,expr1, expr2 ) -> sprintf "EBinOp(%s,%s,%s)" (pprint_binary binary_op) (pprint_expression expr1) (pprint_expression expr2)
  | ECall (identifier,exprs ) -> sprintf "ECall(%s, {%s})" (pprint_identifier identifier) (pprint_expressions exprs)
  | ENew (type_id,expression ) -> sprintf "ENew(%s, %s)" (pprint_type type_id) (pprint_expression expression)
  | EArrayAccess (identifier,expression, field_opt ) -> match field_opt with 
    |None -> sprintf "EArrayAccess(%s,%s,)" (pprint_identifier identifier) (pprint_expression expression)
    |Some(f) -> sprintf "EArrayAccess(%s,%s,%s)" (pprint_identifier identifier) (pprint_expression expression) (pprint_identifier f)
and pprint_expressions lst = 
  let rec work es acc = match es with
    | [] -> String.concat " " (List.rev acc)
    | h::tail -> work tail ((pprint_expression h)::acc) in
  work lst []


let rec pprint_stmts tree= 
  let rec pprint_stmt stmt = 
    let (_, t) = stmt in 
    match t with
    | SExpr(expression) -> sprintf "SExpr(%s)" (pprint_expression expression)
    | SVarAssign (identifier, expression) -> sprintf "SVarAssign(%s, %s)" (pprint_identifier identifier) (pprint_expression expression)
    | SScope ( stmt_lst) -> sprintf "SScope({\n%s\n})" (pprint_stmts stmt_lst)
    | SVarDef (type_id, identifier, expression) -> sprintf "SVarDef(%s, %s, %s)" (pprint_type type_id) (pprint_identifier identifier) (pprint_expression expression)
    | SIf (expression,statement, else_stmt_opt) -> (match else_stmt_opt with
      | None -> sprintf "SIf(%s, \n%s, )" (pprint_expression expression) (pprint_stmt statement)
      | Some(else_branch) -> sprintf "SIf(%s, \n%s, \n%s)" (pprint_expression expression) (pprint_stmt statement) (pprint_stmt else_branch))
    | SWhile (expression, while_body_stmt) -> sprintf "SWhile(%s, \n%s)" (pprint_expression expression) (pprint_stmt while_body_stmt) 
    | SBreak -> "SBreak"
    | SReturn(ret_expr_opt) -> (match ret_expr_opt with
      |None -> "SReturn()"
      |Some(r) -> sprintf "SReturn(%s)" (pprint_expression r) )
    | SDelete (identifier) -> sprintf "SDelete(%s)" (pprint_identifier identifier)
    | SArrayAssign(identifier, index_expr, field_opt, expression) ->  match field_opt with 
      |None->  sprintf "SArrayAssign(%s,%s,,%s)" (pprint_identifier identifier) (pprint_expression index_expr) (pprint_expression expression) 
      | Some(f)-> sprintf "SArrayAssign(%s,%s,%s,%s)" (pprint_identifier identifier) (pprint_expression index_expr) (pprint_identifier f) (pprint_expression expression) in
    let rec eat_stmts lst acc = match lst with 
    | [] -> String.concat "\n" (List.rev acc)
    | h::tail -> eat_stmts tail ((pprint_stmt h)::acc) in
    eat_stmts tree []

let pprint_global tree = 
    let (_, t) = tree in 
    match t with
  | GFuncDef(type_id,identifier,parameters, stmt)  -> sprintf "GFuncDef(%s,%s,%s, \n%s)" (pprint_type type_id) (pprint_identifier identifier) (pprint_parameters parameters) (pprint_stmts [stmt])  
  | GFuncDecl(type_id,identifier,parameters) -> sprintf "GFuncDecl(%s,%s,%s)" (pprint_type type_id) (pprint_identifier identifier) (pprint_parameters parameters)
  | GVarDef(type_id,identifier,expression) -> sprintf "GVarDef(%s,%s,%s)" (pprint_type type_id) (pprint_identifier identifier) (pprint_expression expression) 
  | GVarDecl(type_id,identifier) -> sprintf "GVarDecl(%s,%s)" (pprint_type type_id) (pprint_identifier identifier)
  | GStruct (identifier,fields) -> sprintf "GStruct(%s,%s)" (pprint_identifier identifier) (pprint_fields fields) 


let rec pprint_program tree = match tree with 
  | GlobTerminal -> ""
  | ConsGlob(_,glob, program) -> pprint_global glob ^ "\n\n" ^ (pprint_program program)


(* ------------------------------ pprint control flow intermediate representation -------------------*)
let t = {line = -1; start_p = -1; end_p = -1}

let pprint_ir_id id = sprintf "[ID %d]" id 

let rec pprint_ir_expression tree = 
  match tree with 
  | IREVar(identifier) -> sprintf "EVar(%s)" (pprint_ir_id identifier) 
  | IREInt ( n ) -> sprintf "EInt(%d)" n
  | IREChar (c ) -> sprintf "EChar(\'%c\')" c 
  | IREString (s ) -> sprintf "EString(\"%s\")" s
  | IREUnOp (unary_op,expression ) -> sprintf "EUnOp(%s, %s)" (pprint_unary (t,unary_op)) (pprint_ir_expression expression)
  | IREBinOp (binary_op,expr1, expr2 ) -> sprintf "EBinOp(%s,%s,%s)" (pprint_binary (t,binary_op)) (pprint_ir_expression expr1) (pprint_ir_expression expr2)
  | IRECall (identifier,exprs ) -> sprintf "ECall(%s, {%s})" (pprint_identifier (t,identifier)) (pprint_ir_expressions exprs)
  | IRENew (type_id,expression ) -> sprintf "ENew(%s, %s)" (pprint_type (t,type_id)) (pprint_ir_expression expression)
  | IREArrayAccess (var_key, ir_expr) -> sprintf "EArrayAccess(%s, %s)" (pprint_ir_id var_key) (pprint_ir_expression ir_expr)
and pprint_ir_expressions lst = 
  let rec work es acc = match es with
    | [] -> String.concat " " (List.rev acc)
    | h::tail -> work tail ((pprint_ir_expression h)::acc) in
  work lst []


let pprint_ir_end ir_end = match ir_end with 
  | IRSReturn (None) -> "IRSReturn()"
  | IRSReturn(Some(r)) -> sprintf "IRSReturn(%s)" (pprint_ir_expression r)
  | IRSBranch(e,bid1,bid2) -> sprintf "IRSBranch(%s,%d,%d)" (pprint_ir_expression e) bid1 bid2
  | IRSJump(bid) -> sprintf "IRSJump(%d)" bid
  | IRSNoOp -> "IRSNoOp"

let pprint_ir_stmts ir_stmts = 
    let rec pprint_ir_stmt stmt = match stmt with 
      | IRSCall(e) -> sprintf "IRSCall(%s)" (pprint_ir_expression e)
      | IRSVarDeclare(tpe, id) -> sprintf "IRSVarDecl(%s, %s)" (pprint_ir_id id) (pprint_type (t,tpe))
      | IRSVarAssign(id,e) -> sprintf "IRSVarAssign(%s,%s)" (pprint_ir_id id) (pprint_ir_expression e) 
      | IRSDelete (var_key) -> sprintf "IRSDelete(%s)" (pprint_ir_id var_key) 
      | IRSArrayAssign (var_key, ir_expr1, ir_expr2) -> sprintf "IRSArrayAssign(%s, %s, %s)" (pprint_ir_id var_key) (pprint_ir_expression ir_expr1) (pprint_ir_expression ir_expr2) in 
    let rec eat_stmts stmts acc = match stmts with 
    | [] -> String.concat "\n" (List.rev acc)
    | h::tail -> eat_stmts tail ((pprint_ir_stmt h)::acc) in 
    eat_stmts ir_stmts []

let pprint_ir_blocks ir_blocks = 
  let rec pprint_ir_block (block_key, (ir_stmts, ir_end))= sprintf "IRBlock({%d,\n%s\n%s\n})" block_key (pprint_ir_stmts ir_stmts) (pprint_ir_end ir_end) in 
  let rec eat_blocks lst acc = match lst with 
    | [] -> String.concat "\n" (List.rev acc)
    | h::tail -> eat_blocks tail ((pprint_ir_block h)::acc) in 
  eat_blocks ir_blocks []

let pprint_ir_global = function 
  | IRFunc(id,tpe, params, ir_blocks) -> sprintf "GFuncDef(%s, %s, %s,{\n%s})" (pprint_type (t,tpe)) (pprint_identifier (t,id)) (String.concat ", " (List.map pprint_ir_id params)) (pprint_ir_blocks ir_blocks)
  | IRDecl -> "DECLARATION"
  | IRVarDef -> "VARDEF"


let pprint_ir_cfg ir_cfg = 
  let rec pprint_ir_globals = function 
    | [] -> ""
    | h::tail -> pprint_ir_global h ^ "\n\n" ^ (pprint_ir_globals tail) in 
  pprint_ir_globals ir_cfg




(* ---------------------- pprint assembly ---------------------*)


let pprint_assembly_unop = function 
  | Inc -> "inc"
  | Dec -> "dec"
  | Push -> "push"
  | Pop -> "pop"
  | Mul -> "imul"
  | Div -> "idiv"
  | SetLt -> "setl"
  | SetGt -> "setg" 
  | SetLeq -> "setle" 
  | SetGeq -> "setge" 
  | SetEq -> "sete" 
  | SetNeq -> "setne"
  | BitNot -> "not"
  | Neg -> "neg"
let pprint_assembly_binop = function 
  | Add -> "add"
  | Sub -> "sub"
  | Mov -> "mov"
  | Cmp -> "cmp"
  | BinAnd -> "and"
  | BinOr -> "or"
  | ShiftLeft -> "shl"
  | ShiftRight -> "shr"
  | MovZx -> "movzx"

let str_size = function
  | Byte -> "byte"
  | Word -> "word" 
  | DWord -> "dword" 
  | QWord -> "qword"

let pprint_assembly_register_ext = function 
  | RAX,Byte -> "al"
  | RBX,Byte -> "bl"  
  | RCX,Byte -> "cl" 
  | RDX,Byte -> "dl" 
  | RSI,Byte -> "sil" 
  | RDI,Byte -> "dil" 
  | RBP,Byte -> "bpl" 
  | RSP,Byte -> "spl" 
  | R8,Byte -> "r8b" 
  | R9,Byte -> "r9b" 
  | R10,Byte -> "r10b" 
  | R11,Byte -> "r11b" 
  | R12,Byte -> "r12b" 
  | R13,Byte -> "r13b"
  | R14,Byte -> "r14b" 
  | R15,Byte -> "r15b"
  | RAX,QWord -> "rax"
  | RBX,QWord -> "rbx"  
  | RCX,QWord -> "rcx" 
  | RDX,QWord -> "rdx" 
  | RSI,QWord -> "rsi" 
  | RDI,QWord -> "rdi" 
  | RBP,QWord -> "rbp" 
  | RSP,QWord -> "rsp" 
  | R8,QWord -> "r8" 
  | R9,QWord -> "r9" 
  | R10,QWord -> "r10" 
  | R11,QWord -> "r11" 
  | R12,QWord -> "r12" 
  | R13,QWord -> "r13"
  | R14,QWord -> "r14" 
  | R15,QWord -> "r15"

let pprint_assembly_register = function 
  | RAX -> "rax"
  | RBX -> "rbx"  
  | RCX -> "rcx" 
  | RDX -> "rdx" 
  | RSI -> "rsi" 
  | RDI -> "rdi" 
  | RBP -> "rbp" 
  | RSP -> "rsp" 
  | R8 -> "r8" 
  | R9 -> "r9" 
  | R10 -> "r10" 
  | R11 -> "r11" 
  | R12 -> "r12" 
  | R13  -> "r13"
  | R14 -> "r14" 
  | R15 -> "r15"

let pprint_assembly_operand = function
  | Imm(n) -> sprintf "%d" n
  | Register(r) -> pprint_assembly_register_ext r
  | MemoryAddress(size, r, None, scale, term) -> sprintf "%s[%s + %d]" (str_size size) (pprint_assembly_register_ext (r,QWord)) term 
  | MemoryAddress(size, r, Some(r2), scale, term) -> sprintf "%s[%s + %d * %s + %d]" (str_size size) (pprint_assembly_register_ext (r,QWord)) scale (pprint_assembly_register_ext (r2,QWord)) term 
  | Label(l) -> l

let rec pprint_assembly_instrs instrs acc = match instrs with 
    | [] -> String.concat "\n" (List.rev acc)
    | UnaryOp(unop, operand)::tail -> pprint_assembly_instrs tail ((sprintf "\t%s\t%s" (pprint_assembly_unop unop) (pprint_assembly_operand operand))::acc)
    | BinaryOp(binop, operand1, operand2)::tail ->  pprint_assembly_instrs tail ((sprintf "\t%s\t%s,%s" (pprint_assembly_binop binop) (pprint_assembly_operand operand1) (pprint_assembly_operand operand2))::acc)
    | Call(identifier)::tail ->  pprint_assembly_instrs tail ((sprintf "\tcall\t$%s" identifier)::acc)

let pprint_assembly_jbinop = function 
  | Jl -> "jl"
  | Jg -> "jg"
  | Jne -> "jne"

let pprint_assembly_label fid bid = sprintf "%s_%d" fid bid 

let pprint_assembly_blockend func_label = function 
  | Ret -> "\tret"
  | Jmp(bid) -> sprintf "\tjmp\t%s" (pprint_assembly_label func_label bid)
  | NoOp -> "\tnop"
  | Branch(jbinop, bid1, bid2) -> sprintf "\t%s\t%s\n\tjmp\t%s" (pprint_assembly_jbinop jbinop) (pprint_assembly_label func_label bid1) (pprint_assembly_label func_label bid2)

let rec pprint_assembly_blocks func_label blocks acc = match blocks with 
  | [] -> String.concat "\n" (List.rev acc)
  | AsmBlock(bid, (instrs, blockend))::tail -> 
    let instr_str = pprint_assembly_instrs instrs [] in 
    pprint_assembly_blocks func_label tail ((sprintf "%s:\n%s\n%s\n" (pprint_assembly_label func_label bid) instr_str (pprint_assembly_blockend func_label blockend))::acc)

let rec pprint_assembly_functions funcs acc = match funcs with 
  | [] -> String.concat "\n" (List.rev acc)
  | AsmFunc(id, blocks)::tail -> 
      pprint_assembly_functions tail ((sprintf "%s:\n%s" id (pprint_assembly_blocks id blocks []))::acc)

let rec pprint_assembly_external funcs acc = match funcs with 
  | [] -> String.concat "\n" (List.rev ("\t\textern\tmalloc"::"\t\textern\tfree"::acc)) (*always include the heap-helpers*)
  | id::tail -> pprint_assembly_external tail ((sprintf "\t\textern\t%s" id)::acc)

let rec pprint_assembly_strings acc = 
  let rec sanitize_string curr_acc acc = function
  | [] -> String.concat "," (List.rev ((List.rev ('\"'::curr_acc) |> List.to_seq |> String.of_seq)::acc)) 
  | '\\'::'n'::tail -> sanitize_string ['\"']  ("10"::(List.rev ('\"'::curr_acc) |> List.to_seq |> String.of_seq)::acc) tail 
  | '\\'::'t'::tail -> sanitize_string ['\"'] ("9"::(List.rev ('\"'::curr_acc) |> List.to_seq |> String.of_seq)::acc) tail 
  | '\\'::'\\'::tail -> sanitize_string ['\"'] ("92"::(List.rev ('\"'::curr_acc) |> List.to_seq |> String.of_seq)::acc) tail 
  | '\\'::'\''::tail -> sanitize_string ['\"'] ("39"::(List.rev ('\"'::curr_acc) |> List.to_seq |> String.of_seq)::acc) tail  
  | '\\'::'\"'::tail -> sanitize_string ['\"'] ("34"::(List.rev ('\"'::curr_acc) |> List.to_seq |> String.of_seq)::acc) tail 
  | c::tail -> sanitize_string ((c)::curr_acc) acc tail 
in 
  function 
    | [] -> String.concat "\n" (List.rev acc)
    | (str,lbl)::tail -> pprint_assembly_strings ((sprintf "%s\tdb\t%s,0" lbl (sanitize_string ['\"'] [] (List.init (String.length str) (String.get str))))::acc) tail

let rec pprint_assembly_global_vars acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | (_,lbl,Some(n))::tail -> pprint_assembly_global_vars ((sprintf "%s\tdq\t%d" lbl n)::acc) tail
    | (_,lbl,None)::tail -> pprint_assembly_global_vars acc tail

let pprint_assembly funcs extern_funcs global_strings global_vars= 
  sprintf "\t\tglobal \tmain\n%s\n\t\tsection .rodata\n\n%s\n\n\t\tsection .data\n\n%s\n\n\t\tsection .text \n\n%s" (pprint_assembly_external extern_funcs []) (pprint_assembly_strings [] global_strings) (pprint_assembly_global_vars [] global_vars) (pprint_assembly_functions funcs []) 