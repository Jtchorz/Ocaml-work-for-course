type unop = UnOpMinus | UnOpBitFlip | UnOpNegation
type binop = BopAdd | BopSub | BopMult | BopDiv | BopModulo | BopGreater | BopLesser
  | BopGreaterEq | BopLesserEq | BopEqual | BopNotEq | BopBitAnd | BopBitOr | BopAnd 
  | BopOr | BopShiftLeft | BopShiftRight

type ty = TVoid | TInt | TChar 
  | TIdent of string 
  | TPoint of ty 
 
type expr = EVar of string | EInt of int | EChar of char | EString of string 
  | EBinOp of binop * expr * expr | EUnOp of unop * expr | ECall of string * expr list
  | ENew of ty * expr | EArrayAccess of string * expr * string option 

type stmt = SExpr of expr | SVarDef of ty * string * expr | SVarAssign of string * expr
  | SArrayAssign of string * expr * string option * expr | SScope of stmt list | SIf of expr * stmt * stmt option
  | SWhile of expr * stmt | SBreak | SReturn of expr option | SDelete of string

type global = GFuncDef of ty * string * (ty * string) list * stmt | GFuncDecl of ty * string * (ty * string) list 
  | GVarDef of ty * string * expr | GVarDecl of ty * string | Gstruct of string * (ty * string) list 

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
    Buffer.add_string buf (char_print c) ) s;
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
  | BopLesser -> ">="
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
  | TVoid -> "Tvoid"
  | TInt -> "TInt"
  | TChar -> "Tchar"
  | TIdent(s) -> "TIdent(" ^ (string_print s) ^ ")"
  | TPoint(t) -> "TPoint(" ^ (pprint_ty t) ^ ")" 

let rec pprint_expr = function
  | EVar(s) -> "EVar(" ^ (string_print s) ^ ")"
  | EInt(n) -> "EInt(" ^ (string_of_int n) ^ ")" 
  | EChar(c) -> "EChar(" ^ (char_print c ) ^ ")"
  | EString(s) ->  "Estring(" ^ (string_print s) ^ ")"
  | EBinOp(bop, e1, e2)  -> "EBinOp(" ^ (pprint_binop bop) ^ "," ^ (pprint_expr e1) ^ "," ^ (pprint_expr e2) ^ ")"
  | EUnOp(uop, e) ->  "EUnOp(" ^ (pprint_unop uop) ^ "," ^ (pprint_expr e) ^ ")"
  | ECall(s, l) -> "ECall(" ^ (string_print s) ^ "," ^ "{" ^ (String.concat " " (List.map pprint_expr l)) ^ "})"
  | ENew(t, e) ->  "ENew(" ^ (pprint_ty t) ^ "," ^ (pprint_expr e) ^ ")"
  | EArrayAccess(s, e, sopt) -> "EArrayAccess(" ^ (string_print s) ^ "," ^ (pprint_expr e) ^ "," ^ 
    (match sopt with
      | Some(s) -> string_print s
      | None -> ""
      ) ^ ")"
let rec pprint_stmt = function
  | SExpr(e) -> "SExpr(" ^ (pprint_expr e ) ^ ")"
  | SVarDef(t, s, e) -> "SVarDef(" ^ (pprint_ty t) ^ "," ^ (string_print s) ^ "," ^ (pprint_expr e) ^ ")"
  | SVarAssign(s,e) -> "SVarAssign("  ^ (string_print s) ^ "," ^ (pprint_expr e) ^ ")"
  | SArrayAssign(s,e1,sopt,e2) -> "SArrayAssign(" ^ (string_print s) ^ "," ^ (pprint_expr e1) ^ "," ^ 
    (match sopt with
      | Some(s) -> string_print s
      | None -> ""
      ) ^ "," ^ (pprint_expr e2 ) ^ ")"
  | SScope(l) ->  "SScope({" ^ (String.concat " " (List.map pprint_stmt l)) ^ "})"
  | SIf(e,st,opst) -> "SIf(" ^ (pprint_expr e) ^ "," ^ (pprint_stmt st) ^ "," ^ 
    (match opst with
      | Some(st1) -> pprint_stmt st1
      | None -> ""
      ) ^ ")"
  | SWhile(e, st) ->  "SWhile(" ^ (pprint_expr e) ^ "," ^  (pprint_stmt st) ^ ")"
  | SBreak -> "SBreak"
  | SReturn(eop) -> "SReturn(" ^  
    (match eop with
      | Some(e) -> pprint_expr e
      | None -> ""
      ) ^ ")" 
  | SDelete(s) -> "SDelete(" ^ (string_print s) ^ ")"