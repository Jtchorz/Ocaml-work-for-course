(*
NOTE: This parser gives rise to a shift/reduce conflict when parsing 
nested If-Else statements. However, as specified in §6.3 in the Menhir 
manual, these issues are automaticall resolved in favour of shifting, 
which in this case entails that the Else clause will belong to the inner 
most If-statement, which is the intended behaviour. 

Menhir manual: https://pauillac.inria.fr/~fpottier/menhir/manual.html#sec%3Aconflicts
*)

%{
    open Ast

    let get_meta_from_global g prev_meta = match g with 
        | GlobTerminal -> prev_meta 
        | ConsGlob(t,_,_) -> t
%}

%token <Ast.tmd> LParen RParen LBracket RBracket LCurly RCurly Comma Dot SemiC Not BinaryNot Plus Minus Multiplication Division Modulo Inc Dec LT GT LEQ GEQ Equality Inequality BinaryAnd BinaryOr LogicalAnd LogicalOr ShiftLeft ShiftRight Assign Void TypeInt TypeChar TypeStruct For While If Else Break Return Delete Extern New
%token <Ast.tmd * string> Identifier 
%token <Ast.tmd* int> IntConstant 
%token <Ast.tmd* string> CharConstant 
%token <Ast.tmd* string> StringConstant
%token <Ast.tmd> EOF
%token <Ast.tmd * string> ErrorLex

%left LogicalOr
%left LogicalAnd
%left BinaryOr
%left BinaryAnd
%left Equality Inequality
%left LT GT LEQ GEQ
%left ShiftLeft ShiftRight
%left Plus Minus
%left Multiplication Division Modulo
%nonassoc Unary

%start init
%type <program> init

%%

init: 
    | g=global i=init
        {
            let mm = get_meta_from_global i (fst g) in 
            let meta = {(fst g) with end_p=mm.end_p} in 
            ConsGlob(meta,g, i)
        }
    | EOF
        {GlobTerminal}

global:
    | t=type_id Identifier LParen parameters statement
        {
            let meta = {(fst t) with end_p = (fst $5).end_p} in 
            (meta, GFuncDef(t, $2, $4,$5))
        }
    | Extern type_id Identifier LParen parameters SemiC
        {
            let meta = {$1 with end_p = $6.end_p} in 
            (meta, GFuncDecl($2, $3, $5))
        }
    | type_id Identifier LParen parameters SemiC
        {
            let meta = {(fst $1) with end_p = $5.end_p} in 
            (meta, GFuncDecl($1, $2, $4))
            
        }
    | type_id Identifier Assign expression SemiC
        {
            let meta = {(fst $1) with end_p = $5.end_p} in 
            (meta, GVarDef($1, $2, $4))
            }
    | Extern type_id Identifier SemiC
        {
            let meta = {$1 with end_p = $4.end_p} in 
            (meta, GVarDecl($2, $3))
            }
    | type_id Identifier SemiC
        {
            let meta = {(fst $1) with end_p = $3.end_p} in 
            (meta, GVarDecl($1, $2))
        }
    | TypeStruct Identifier LCurly fields  SemiC
        {
            let meta = {$1 with end_p = $5.end_p} in 
            (meta, GStruct($2, $4))
        }


statement_list:
    | RCurly {($1,[])}
    | statement statement_list {(fst $2, $1::(snd $2))}


assign:
    | Identifier LParen arguments RParen {
        let meta = {(fst $1) with end_p=$4.end_p} in 
        (meta, SExpr((meta, ECall($1, $3))))
        } 
    | Identifier Assign expression {
        let meta = {(fst $1) with end_p=(fst $3).end_p} in 
        (meta, SVarAssign($1, $3))
        }
    | Identifier LBracket expression RBracket Assign expression {
        let meta = {(fst $1) with end_p=(fst $6).end_p} in 
        (meta, SArrayAssign($1, $3, None, $6))
        }
    | Identifier LBracket expression RBracket Dot Identifier Assign expression {
        let meta = {(fst $1) with end_p=(fst $8).end_p} in 
        (meta, SArrayAssign($1, $3, Some($6), $8))
        
        }
    | Identifier Inc {
        let meta = {(fst $1) with end_p=$2.end_p} in 
        (meta, SVarAssign($1, ($2, EBinOp(($2,PlusOp), (fst $1, EVar($1)), ($2,EInt(1))))))
        }
    | Identifier LBracket expression RBracket Inc {
        let meta = {(fst $1) with end_p=$5.end_p} in 
        (meta, SArrayAssign($1, $3, None, ($5, EBinOp(($5,PlusOp), (fst $1, EArrayAccess($1, $3, None)), ($5,EInt(1))))))
        }
    | Identifier LBracket expression RBracket Dot Identifier Inc {
        let meta = {(fst $1) with end_p=$7.end_p} in 
        (meta, SArrayAssign($1, $3, Some($6), ($7, EBinOp(($7,PlusOp), (fst $1, EArrayAccess($1, $3, Some($6))), ($7,EInt(1))))))

        }
    | Identifier Dec {
        let meta = {(fst $1) with end_p=$2.end_p} in 
        (meta, SVarAssign($1, ($2, EBinOp(($2,MinusOp), (fst $1, EVar($1)), ($2,EInt(1))))))
        }
    | Identifier LBracket expression RBracket Dec {
        let meta = {(fst $1) with end_p=$5.end_p} in 
        (meta, SArrayAssign($1, $3, None, ($5, EBinOp(($5,MinusOp), (fst $1, EArrayAccess($1, $3, None)), ($5,EInt(1))))))
        }
    | Identifier LBracket expression RBracket Dot Identifier Dec {
        let meta = {(fst $1) with end_p=$7.end_p} in 
        (meta, SArrayAssign($1, $3, Some($6), ($7, EBinOp(($7,MinusOp), (fst $1, EArrayAccess($1, $3, Some($6))), ($7,EInt(1))))))

        }

statement:
    | Identifier LParen arguments RParen SemiC {
        ({(fst $1) with end_p = $5.end_p}, SExpr((fst $1, ECall($1, $3))))
        
        }
    | Identifier Assign expression SemiC {
        ({(fst $1) with end_p = $4.end_p}, SVarAssign($1,$3))
        }
    | type_id Identifier Assign expression SemiC {
        ({(fst $1) with end_p = $5.end_p}, SVarDef($1,$2,$4))
        }
    | LCurly statement_list {
        ({$1 with end_p = (fst $2).end_p}, SScope(snd $2))
        }
    | If LParen expression RParen statement 
        {
        ({$1 with end_p=(fst $5).end_p}, SIf($3, $5, None))
        }
    | If LParen expression RParen statement Else statement  
        {
            ({$1 with end_p = (fst $7).end_p}, SIf($3,$5,Some($7)))
            }
    | While LParen expression RParen statement {
        ({$1 with end_p=(fst $5).end_p}, SWhile($3,$5))
        }
    | Break SemiC {
        ({$1 with end_p=$2.end_p}, SBreak)}
    | Return SemiC {
        ({$1 with end_p=$2.end_p}, SReturn(None))}
    | Return expression SemiC {
        ({$1 with end_p=$3.end_p}, SReturn(Some($2)))
        }
    | Delete LBracket RBracket Identifier SemiC {
        ({$1 with end_p=$5.end_p}, SDelete($4))
        }
    | Identifier Inc SemiC {
        ({(fst $1) with end_p=$3.end_p}, SVarAssign($1, ($2,EBinOp(($2,PlusOp), ($2,EVar($1)), ($2,EInt(1))))))
        }
    | Identifier Dec SemiC {({(fst $1) with end_p=$3.end_p}, SVarAssign($1, ($2,EBinOp(($2,MinusOp), ($2,EVar($1)), ($2,EInt(1))))))}
    | Identifier LBracket expression RBracket Assign expression SemiC 
        {
            ({(fst $1) with end_p=$7.end_p}, SArrayAssign($1, $3, None, $6))
        }
    | Identifier LBracket expression RBracket Dot Identifier Assign expression SemiC 
        {
            ({(fst $1) with end_p=$7.end_p}, SArrayAssign($1, $3, Some($6), $8))
        }
    | Identifier LBracket expression RBracket Inc SemiC 
        {
            ({(fst $1) with end_p=$6.end_p}, SArrayAssign($1, $3, None, ($5,EBinOp(($5,PlusOp), ($2,EArrayAccess($1, $3, None)), ($2,EInt(1))))))
            }
    | Identifier LBracket expression RBracket Dot Identifier Inc SemiC 
        {
            ({(fst $1) with end_p=$8.end_p}, SArrayAssign($1, $3, Some($6), ($5,EBinOp(($5,PlusOp), ($2,EArrayAccess($1, $3, Some($6))), ($2,EInt(1))))))
        }
    | Identifier LBracket expression RBracket Dec SemiC 
        {({(fst $1) with end_p=$6.end_p}, SArrayAssign($1, $3, None, ($5,EBinOp(($5,MinusOp), ($2,EArrayAccess($1, $3, None)), ($2,EInt(1))))))}
    | Identifier LBracket expression RBracket Dot Identifier Dec SemiC 
        {({(fst $1) with end_p=$8.end_p}, SArrayAssign($1, $3, Some($6), ($5,EBinOp(($5,MinusOp), ($2,EArrayAccess($1, $3, Some($6))), ($2,EInt(1))))))}
    | For LParen a=assign SemiC expression SemiC b=assign RParen statement
        {
            let meta = {$1 with end_p=(fst $9).end_p} in 
            (meta,SScope([a; ($1,SWhile($5, ((fst $9), SScope([$9; b]))))]))
        }
    | For LParen type_id Identifier Assign expression SemiC expression SemiC b=assign RParen statement
        {
            
            let meta = {$1 with end_p=(fst $12).end_p} in 
            (meta,SScope([((fst $3, SVarDef($3,$4,$6))); ($1,SWhile($8, ((fst $12), SScope([$12; b]))))]))
        }

expression:
    | LParen expression RParen { ({$1 with end_p = $3.end_p}, snd $2) }
    | Identifier LParen arguments RParen {
        ({(fst $1) with end_p = $4.end_p}, ECall($1, $3))
        }
    | Identifier
        {(fst $1, EVar($1))}
    | IntConstant
        {(fst $1, EInt(snd $1))}
    | CharConstant{(fst $1, EChar(snd $1))}
    | StringConstant {(fst $1, EString(snd $1))}
    | expression binary_op expression
        {
            ({(fst $1) with end_p = (fst $3).end_p}, EBinOp($2, $1, $3))
            }
    | unary_op expression %prec Unary
        {
            ({(fst $1) with end_p = (fst $2).end_p}, EUnOp($1, $2))
            }
    | New type_id LBracket expression RBracket {
        ({ $1 with end_p = $5.end_p}, ENew($2, $4))
        }
    | Identifier LBracket expression RBracket {
        ({(fst $1) with end_p = $4.end_p}, EArrayAccess($1, $3, None))
        }
    | Identifier LBracket expression RBracket Dot Identifier
        {
            ({(fst $1) with end_p = (fst $6).end_p}, EArrayAccess($1, $3, Some($6)))
            }

type_id:
    | Void {($1, TVoid)}
    | TypeInt {($1, TInt)}
    | TypeChar {($1, TChar)}
    | Identifier {(fst $1, Identifier($1))}
    | type_id Multiplication {({(fst $1) with end_p = $2.end_p}, TPoint($1))}


%inline binary_op:
    | Plus {($1, PlusOp)}
    | Minus {($1, MinusOp)}
    | Multiplication {($1, MultOp)}
    | Division {($1, DivOp)}
    | Modulo {($1, ModOp)}
    | LT {($1, LTOp)}
    | GT {($1, GTOp)}
    | LEQ {($1, LEQOp)}
    | GEQ {($1, GEQOp)}
    | Equality {($1, EqOp)}
    | Inequality {($1, NeqOp)}
    | BinaryAnd {($1, BinAndOp)}
    | BinaryOr {($1, BinOrOp)}
    | LogicalAnd {($1, LogAndOp)}
    | LogicalOr {($1, LogOrOp)}
    | ShiftLeft {($1, ShiftLeftOp)}
    | ShiftRight {($1, ShiftRightOp)}

unary_op:
    | Not {($1, LogNotOp)}
    | BinaryNot {($1, BinNotOp)}
    | Minus {($1, UnaryMinOp)}


fields:
    RCurly {[]}
    | type_id Identifier RCurly {[({(fst $1) with end_p=$3.end_p}, Field($1,(fst $2, snd $2)))]}
    | type_id Identifier SemiC fields {({(fst $1) with end_p=(fst $2).end_p}, Field($1,(fst $2, snd $2)))::$4}

parameters:
    RParen {[]}
    | type_id Identifier RParen {[({(fst $1) with end_p=(fst $2).end_p}, Param($1,(fst $2, snd $2)))]}
    | type_id Identifier Comma parameters {({(fst $1) with end_p=(fst $2).end_p}, Param($1,(fst $2, snd $2)))::$4}

arguments:
    {[]}
    | expression {[$1]}
    | expression Comma arguments {$1::$3}
