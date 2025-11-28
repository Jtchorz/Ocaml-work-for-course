type unop = UnOpMinus | UnOpBitFlip | UnOpNegation
type binop = BopAdd | BopSub | BopMult | BopDiv | BopModulo | BopGreater | BopLesser
  | BopGreaterEq | BopLesserEq | BopEqual | BopNotEq | BopBitAnd | BopBitOr | BopAnd 
  | BopOr | BopShiftLeft | BopShiftRight

type ty = TVoid | TInt | TChar 
  | TIdent of string
  | TPoint of ty
 
type expr = EVar of string * int | EInt of int * int | EChar of char * int | EString of string * int 
  | EBinOp of binop * expr * expr * int | EUnOp of unop * expr * int | ECall of string * expr list * int
  | ENew of ty * expr * int | EArrayAccess of string * expr * string option * int

type stmt = SExpr of expr * int | SVarDef of ty * string * expr * int | SVarAssign of string * expr * int
  | SArrayAssign of string * expr * string option * expr * int | SScope of stmt list * int | SIf of expr * stmt * stmt option * int
  | SWhile of expr * stmt * int | SBreak of int | SReturn of expr option * int | SDelete of string * int

type global = GFuncDef of ty * string * (ty * string) list * stmt * int | GFuncDecl of ty * string * (ty * string) list * int
  | GVarDef of ty * string * expr * int | GVarDecl of ty * string * int | Gstruct of string * (ty * string) list * int

type program =
  | Prog of global list

let char_print ins = let s = (match ins with
  | '\n' -> "\\n" 
  | '\t' -> "\\t" 
  | '\\' -> "\\\\"
  |  '\''-> "\\\'"
  |  '\"' -> "\\\""
  | c -> String.make 1 c ) in 
  "\'" ^ s ^ "\'" 

let string_print s =
  let buf = Buffer.create 20 in 
  String.iter( fun c ->
    Buffer.add_string buf (match c with
      | '\n' -> "\\n" 
      | '\t' -> "\\t" 
      | '\\' -> "\\"
      |  '\''-> "\'"
      |  '\"' -> "\""
      | c -> String.make 1 c 
  ) ) s;
  "\"" ^ Buffer.contents buf ^ "\""


let pprint_unop = function
  | UnOpMinus -> "-"
  | UnOpBitFlip -> "~"
  | UnOpNegation -> "!"

let pprint_binop = function
  | BopAdd -> "+"
  | BopSub -> "-"
  | BopMult -> "*"
  | BopDiv -> "/"
  | BopModulo -> "%"
  | BopGreater -> ">"
  | BopLesser -> "<"
  | BopGreaterEq -> ">="
  | BopLesserEq -> "<="
  | BopEqual -> "=="
  | BopNotEq -> "!="
  | BopBitAnd -> "&"
  | BopBitOr -> "|"
  | BopAnd -> "&&"
  | BopOr -> "||"
  | BopShiftLeft -> "<<"
  | BopShiftRight -> ">>"


  let rec pprint_ty = function
  | TVoid -> "TVoid"
  | TInt -> "TInt"
  | TChar -> "TChar"
  | TIdent(s) -> "TIdent(" ^ (string_print s) ^ ")"
  | TPoint(t) -> "TPoint(" ^ (pprint_ty t) ^ ")" 

let rec pprint_expr = function
  | EVar(s,_) -> "EVar(" ^ (string_print s) ^ ")"
  | EInt(n,_) -> "EInt(" ^ (string_of_int n) ^ ")" 
  | EChar(c,_) -> "EChar(" ^ (char_print c ) ^ ")"
  | EString(s,_) -> "EString(" ^ (string_print s) ^ ")"
  | EBinOp(bop, e1, e2,_)  -> "EBinOp(" ^ (pprint_binop bop) ^ ", " ^ (pprint_expr e1) ^ ", " ^ (pprint_expr e2) ^ ")"
  | EUnOp(uop, e,_) ->  "EUnOp(" ^ (pprint_unop uop) ^ ", " ^ (pprint_expr e) ^ ")"
  | ECall(s, l,_) -> "ECall(" ^ (string_print s) ^ ", " ^ "{" ^ (String.concat " " (List.map pprint_expr l)) ^ "})"
  | ENew(t, e,_) ->  "ENew(" ^ (pprint_ty t) ^ ", " ^ (pprint_expr e) ^ ")"
  | EArrayAccess(s, e, sopt,_) -> "EArrayAccess(" ^ (string_print s) ^ ", " ^ (pprint_expr e) ^ ", " ^ 
    (match sopt with
      | Some(s) -> string_print s
      | None -> ""
      ) ^ ")"
let rec pprint_stmt = function
  | SExpr(e,_) -> "SExpr(" ^ (pprint_expr e ) ^ ")"
  | SVarDef(t, s, e,_) -> "SVarDef(" ^ (pprint_ty t) ^ ", " ^ (string_print s) ^ ", " ^ (pprint_expr e) ^ ")"
  | SVarAssign(s,e,_) -> "SVarAssign("  ^ (string_print s) ^ ", " ^ (pprint_expr e) ^ ")"
  | SArrayAssign(s,e1,sopt,e2,_) -> "SArrayAssign(" ^ (string_print s) ^ ", " ^ (pprint_expr e1) ^ ", " ^ 
    (match sopt with
      | Some(s) -> string_print s
      | None -> ""
      ) ^ ", " ^ (pprint_expr e2 ) ^ ")"
  | SScope(l,_) ->  "SScope({\n" ^ (String.concat " \n" (List.map pprint_stmt l)) ^ "\n})\n"
  | SIf(e,st,opst,_) -> "SIf(" ^ (pprint_expr e) ^ ", " ^ (pprint_stmt st) ^ ", " ^ 
    (match opst with
      | Some(st1) -> pprint_stmt st1
      | None -> ""
      ) ^ ")"
  | SWhile(e, st,_) ->  "SWhile(" ^ (pprint_expr e) ^ ", " ^  (pprint_stmt st) ^ ")"
  | SBreak(_) -> "SBreak"
  | SReturn(eop,_) -> "SReturn(" ^  
    (match eop with
      | Some(e) -> pprint_expr e
      | None -> ""
      ) ^ ")" 
  | SDelete(s,_) -> "SDelete(" ^ (string_print s) ^ ")"

let pairList_print pairList = 
  let buf = Buffer.create 20 in 
    List.iter (fun (ty,s) -> 
      Buffer.add_string buf ( "("^(pprint_ty ty) ^", "^ (string_print s) ^ ")")) pairList;
  "{"^ (Buffer.contents buf) ^"}"

let pprint_global = function
  | GFuncDef(t,s,listTySt,st,_) -> "GFuncDef(" ^ (pprint_ty t) ^ ", " ^ (string_print s) ^ ", " ^ (pairList_print listTySt) ^ ", " ^ (pprint_stmt st) ^ ")" ^ "\n"
  | GFuncDecl(t,s,listTySt,_) -> "GFuncDecl(" ^ (pprint_ty t) ^ ", " ^ (string_print s) ^ ", " ^ (pairList_print listTySt) ^ ")" ^ "\n"
  | GVarDef(t,s,e,_) -> "GVarDef(" ^ (pprint_ty t) ^ ", " ^ (string_print s) ^ ", " ^ (pprint_expr e) ^ ")" ^ "\n"
  | GVarDecl(t, s,_) -> "GVarDecl(" ^ (pprint_ty t) ^ ", " ^ (string_print s) ^ ")" ^ "\n"
  | Gstruct(s, listTySt,_) -> "GStruct(" ^ (string_print s) ^ ", " ^ (pairList_print listTySt) ^ ")" ^ "\n"

let pprint_program = function
  | Prog(globalList) ->  String.concat " " (List.map (pprint_global) globalList)