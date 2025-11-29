type tmd = {
line: int; start_p: int; end_p: int
} 

type identifier = string
type identifier_meta = tmd * identifier
type type_id =
  |TVoid | TInt | TChar 
  | Identifier of identifier_meta | TPoint of type_id_meta
and type_id_meta = tmd * type_id

type parameters = 
  | Param of type_id_meta*identifier_meta
type parameters_meta = tmd*parameters
type fields = 
  | Field of type_id_meta*identifier_meta
type fields_meta = tmd*fields

type unary_op =
  | LogNotOp | BinNotOp | UnaryMinOp
type unary_op_meta = tmd*unary_op

type binary_op = 
  | PlusOp| MinusOp | MultOp | DivOp | ModOp | LTOp | GTOp | LEQOp 
  | GEQOp | EqOp | NeqOp | BinAndOp | BinOrOp | LogAndOp | LogOrOp 
  | ShiftLeftOp | ShiftRightOp
type binary_op_meta = tmd*binary_op


type expression=
  | EVar of identifier_meta
  | EInt of int
  | EChar of string
  | EString of string
  | EUnOp of unary_op_meta*expression_meta 
  | EBinOp of binary_op_meta*expression_meta*expression_meta
  | ECall of identifier_meta*(expression_meta list)
  | ENew of type_id_meta*expression_meta
  | EArrayAccess of identifier_meta*expression_meta*(identifier_meta option)
and expression_meta = tmd*expression

type statement = 
  | SExpr of expression_meta
  | SVarAssign of identifier_meta * expression_meta
  | SScope of (statement_meta list)
  | SVarDef of type_id_meta * identifier_meta * expression_meta
  | SIf of expression_meta*statement_meta*(statement_meta option)
  | SWhile of expression_meta*statement_meta
  | SBreak
  | SReturn of (expression_meta option)
  | SDelete of identifier_meta
  | SArrayAssign of identifier_meta * expression_meta * (identifier_meta option) * expression_meta
and statement_meta = tmd*statement


type global = 
  | GFuncDef of type_id_meta*identifier_meta*(parameters_meta list)*statement_meta
  | GFuncDecl of type_id_meta*identifier_meta*(parameters_meta list)
  | GVarDef of type_id_meta*identifier_meta*expression_meta
  | GVarDecl of type_id_meta*identifier_meta
  | GStruct of identifier_meta*(fields_meta list)
type global_meta = tmd*global

type program = 
  | GlobTerminal
  | ConsGlob of tmd*global_meta*program