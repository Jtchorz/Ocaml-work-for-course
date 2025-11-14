%{
open Ast
%}

%token Break
%token Char
%token Delete
%token Else
%token Extern
%token For
%token If
%token Int
%token New
%token Return
%token Struct 
%token Void
%token While

%token <string> Ident
%token Eof
%token <int> IntConst
%token <char> CharConst
%token <string> StringConst

%token Exclamation "!"
%token Tylde "~"
%token Minus "-"
%token Star "*"
%token FSlash "/"
%token Percent "%"
%token Plus "+"
%token SmallerThan "<"
%token BiggerThan ">"
%token Equal "="
%token And "&"
%token Bar "|"
%token RParent ")"
%token LParent "("
%token SemiColon ";"
%token RCurly "}"
%token LCurly "{"
%token Comma ","
%token RSquare "]"
%token LSquare "["
%token Dot "."

%token ShiftLeft "<<"
%token ShiftRight ">>"
%token SmallerEqual "<="
%token BiggerEqual ">="
%token DoubleEqual "=="
%token NotEqual "!="
%token AndAnd "&&"
%token Or "||"
%token PlusPlus "++"
%token MinusMinus "--"


%left "||"
%left "&&"
%left "|"
%left "&"
%left "==" "!="
%left "<" ">" "<=" ">="
%left "<<" ">>"
%left "+" "-"
%left "*" "/" "%"
%right "!" "~" UMINUS

%start main 

%type <program> main
%type <program> program
%type <global> global
%type <(ty * string) list> params
%type <stmt> stmt
%type <stmt> varassign
%type <stmt> assign
%type <expr> lvalue
%type <expr> expr
%type <ty> ty

%type <global list> list(global)
%type <stmt list> list(stmt)
%type <(ty * string) list> list(terminated(pair(ty,Ident),SemiColon))
%type <expr list> loption(separated_nonempty_list(Comma,expr))
%type <(ty * string) list> loption(separated_nonempty_list(Comma,pair(ty,Ident)))
%type <expr option> option(expr)
%type <string option> option(preceded(Dot,Ident))
%type <stmt option> option(preceded(Else,stmt))
%type <expr list> separated_nonempty_list(Comma,expr)
%type <(ty * string) list> separated_nonempty_list(Comma,pair(ty,Ident))

%%

main:
    | p = program Eof
        { p }

program:
    | l = list(global) 
        { Prog(l) }

global:
    | t = ty; s = Ident "(" listTySt = params ")"  st = stmt  { GFuncDef(t,s,listTySt,st) }
    | Extern t = ty s = Ident "(" listTySt = params ")" ";" { GFuncDecl (t,s,listTySt)}
    | t = ty s = Ident "=" e = expr ";" { GVarDef(t,s,e)}
    | Extern t = ty s = Ident ";" { GVarDecl(t, s) }
    | Struct s = Ident "{" l = list(terminated(pair(ty,Ident),";")) "}" ";" { Gstruct(s,l) }

params:
    | l = separated_list(",", pair(ty, Ident)) { l }

stmt:
    | v = varassign ";" { v }
    | "{" l = list(stmt) "}" { SScope(l) }
    | If "(" e = expr ")" st = stmt opst = option(preceded(Else, stmt)) { SIf(e,st,opst) }
    | While "(" e = expr ")" st = stmt { SWhile(e,st) }
    | Break ";" { SBreak }
    | Return eop = option(expr) ";" { SReturn(eop) }
    | Delete "[" "]" s = Ident ";" { SDelete(s) }
    | For "(" v = varassign ";" e = expr ";" a = assign ")" st = stmt { SScope([v; SWhile(e, SScope([st;a]))]) }

varassign:
    | t = ty s = Ident "=" e = expr { SVarDef(t,s,e) } (*variable def*)
    | a = assign { a }  (*variable assignment etc*)

assign:
    | s = Ident "(" l = separated_list(",",expr) ")" { SExpr(ECall(s,l))}
    | l = lvalue "=" e0 = expr { match l with
            | EArrayAccess(s1,e,sopt) -> SArrayAssign(s1,e,sopt,e0) 
            | EVar(s) -> SVarAssign(s, e0)
            | _ -> failwith "lvalue returned something extra wierd"
          } 

    | l = lvalue "++" { match l with
            | EArrayAccess(s1,e,sopt) -> SArrayAssign(s1,e,sopt,
                EBinOp(BopAdd, EArrayAccess(s1,e,sopt), EInt(1))) 
            | EVar(s) -> SVarAssign(s, EBinOp(BopAdd,EVar(s),EInt(1)))
            | _ -> failwith "lvalue returned something extra wierd"

          } 

    | l = lvalue "--" 
        { match l with
            | EArrayAccess(s1,e,sopt) -> SArrayAssign(s1,e,sopt,
                EBinOp(BopSub, EArrayAccess(s1,e,sopt), EInt(1))) 
            | EVar(s) -> SVarAssign(s, EBinOp(BopSub,EVar(s),EInt(1)))
            | _ -> failwith "lvalue returned something extra wierd"
          }

lvalue:
    | s = Ident { EVar(s) }
    | s1 = Ident "[" e = expr "]" s2 = option(preceded(".", Ident))  { EArrayAccess(s1,e,s2)}

expr: 
    | s = Ident { EVar(s) }
    | n = IntConst { EInt(n) }
    | c = CharConst { EChar(c) }
    | s = StringConst { EString(s) }
    | e1 = expr "+" e2 = expr { EBinOp(BopAdd, e1, e2) }
    | e1 = expr "-" e2 = expr { EBinOp(BopSub, e1, e2) }
    | e1 = expr "*" e2 = expr { EBinOp(BopMult, e1, e2) }
    | e1 = expr "/" e2 = expr { EBinOp(BopDiv, e1, e2) }
    | e1 = expr "%" e2 = expr { EBinOp(BopModulo, e1, e2) }
    | e1 = expr ">" e2 = expr { EBinOp(BopGreater, e1, e2) }
    | e1 = expr "<" e2 = expr { EBinOp(BopLesser, e1, e2) }
    | e1 = expr ">=" e2 = expr { EBinOp(BopGreaterEq, e1, e2) }
    | e1 = expr "<=" e2 = expr { EBinOp(BopLesserEq, e1, e2) }
    | e1 = expr "==" e2 = expr { EBinOp(BopEqual, e1, e2) }
    | e1 = expr "!=" e2 = expr { EBinOp(BopNotEq, e1, e2) }
    | e1 = expr "&" e2 = expr { EBinOp(BopBitAnd, e1, e2) }
    | e1 = expr "|" e2 = expr { EBinOp(BopBitOr, e1, e2) }
    | e1 = expr "&&" e2 = expr { EBinOp(BopAnd, e1, e2) }
    | e1 = expr "||" e2 = expr { EBinOp(BopOr, e1, e2) }
    | e1 = expr "<<" e2 = expr { EBinOp(BopShiftLeft, e1, e2) }
    | e1 = expr ">>" e2 = expr { EBinOp(BopShiftRight, e1, e2) }
    | "!" e = expr { EUnOp(UnOpNegation, e)}
    | "~" e = expr { EUnOp(UnOpBitFlip, e)}
    | "-" e = expr %prec UMINUS { EUnOp(UnOpMinus, e)}
    | s = Ident "(" l = separated_list(",",expr) ")" { ECall(s,l) } 
    | New t = ty "[" e = expr "]" { ENew(t, e) }
    | s = Ident "[" e = expr "]" sopt = option(preceded(".", Ident))  { EArrayAccess(s,e,sopt)}
    | "(" e = expr ")" { e } (*ask abt this*)

ty:
    | Void { TVoid }
    | Int { TInt }
    | Char { TChar }
    | s = Ident { TIdent(s)}
    | t = ty "*" { TPoint(t) }

(*binop: 
    | "+" { BopAdd }
    | "-" { BopSub }
    | "*" { BopMult }
    | "/" { BopDiv }
    | "%" { BopModulo }
    | ">" { BopGreater }
    | "<" { BopLesser }
    | ">=" { BopGreaterEq }
    | "<=" { BopLesserEq }
    | "==" { BopEqual }
    | "!=" { BopNotEq }
    | "&" { BopBitAnd }
    | "|" { BopBitOr }
    | "&&" { BopAnd }
    | "||" { BopOr }
    | "<<" { BopShiftLeft }
    | ">>" { BopShiftRight }


unop:
    | "!" { UnOpNegation }
    | "~" { UnOpBitFlip }
    | "-" %prec UMINUS { UnOpMinus }*)