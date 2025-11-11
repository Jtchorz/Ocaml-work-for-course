type unop = UopMinus | UopBitFlip | UopNegation
type binop = BopAdd | BopSub | BopMult | BopDiv | BopModulo | BopGreater | BopLesser
  | BopGreaterEq | BopLesserEq | BopEqual | BopNotEq | BopBitAnd | BopBinOr | BopAnd 
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

type prog =
  | Prog of global list