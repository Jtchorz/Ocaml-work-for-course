(*define the IR and a pretty printer for it*)
open Ast
type ir_stmt =
  | ISExpr of expr * int 
  | ISVarAssign of string * expr *int
  | ISVarDecl  of string * ty *int
  | ISDelete of string * int
  | ISArrayAssign of string * expr * expr * int

type ir_blockend = 
  | ISReturn of expr option * int
  | ISBranch of expr * string * string *int (*this and lower not used for S but gonna implement just to leave them empty later*)
  | ISJump of string *int


type ir_block =
  | IBlock of string * (ir_stmt list * ir_blockend)*int

type ir_global =
  | IFunc of string * (ty * (ty*string) list * ir_block list)*int
  | IGVarDef of ty * string * expr *int
(*I am putting global vars in here because
I want to implement strings as part of the .data
section, and I don't want to have some of it written
somewhere else, so both will be declared during instruction selection*)
let pprint_ir_stmt = function
  | ISExpr(e,_) -> "ISExpr(" ^(pprint_expr e) ^")\n" 
  | ISVarAssign(s,e,_) -> "ISVarAssign(\""^s^"\", "^(pprint_expr e)^")\n"
  | ISVarDecl(s,t,_) -> "ISVarDecl(\""^s^"\", "^(pprint_ty t)^")\n"
  | ISDelete(s,_)-> "IDelete(\""^s^"\")\n"
  | ISArrayAssign(s,e,eVal,_)-> "ISArrayAssign(\""^s^"\", "^(pprint_expr e)^", "^(pprint_expr eVal)^")\n"

let pprint_ir_blockend = function
  | ISReturn(eop,_) ->(
    match eop with
    | Some(e) ->"ISReturn("^(pprint_expr e)^")\n"
    | None -> "ISReturn()\n"
  )
  | ISBranch(e,s1,s2,_) -> "ISBranch("^(pprint_expr e)^",\n"^s1^", "^s2^")\n"
  | ISJump(s,_) -> "ISJump("^s^")\n"

let rec pprint_stmt_list = function
  | st::restlist -> (pprint_ir_stmt st)^(pprint_stmt_list restlist)
  | [] -> ""


let pprint_ir_block = function
  | IBlock(s,(stlist,blockend),_) -> "IBlock({"^s^",\n"^ (pprint_stmt_list stlist) ^(pprint_ir_blockend blockend)^"})\n"

let rec pprint_param_list = function
  | (t,s)::restlist -> ((pprint_ty t)^" "^s)^", "^(pprint_param_list restlist)
  | [] -> ""

let rec pprint_block_list = function
  | b::restlist -> (pprint_ir_block b)^(pprint_block_list restlist)
  | [] -> ""

let pprint_ir_global = function
  | IFunc(s, (t, listTySt, blist),_) -> "IFunc("^(pprint_ty t)^", \""^s^"\", {"^(pprint_param_list listTySt)^"},\n"^(pprint_block_list blist)^")\n"
  | IGVarDef(t,s,e,_) -> "IGVarDef("^(pprint_ty t)^", \""^s^"\", "^pprint_expr e^")\n"



let pprint_ir_global_list iList =
    let rec work acc = function
    | iGlob::restlist -> 
      let nacc = 
        acc^(pprint_ir_global iGlob)
      in 
      work nacc restlist
    | [] -> acc
    in work "" iList