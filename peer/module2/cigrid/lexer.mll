

{
    open Parser
    open Ast
    exception ErrorLex of tmd*string

    (* generete metadata record storing the line number, startposition and endposition of the token *)
    let meta (lbuf:Lexing.lexbuf) =  {line= (lbuf.lex_curr_p.pos_lnum); start_p = Lexing.lexeme_start lbuf; end_p=Lexing.lexeme_end lbuf}
}

rule token_rule = parse
    [' ' '\t' '\r'] 
    | ("//" | "#" )[^'\n']* 
        {token_rule lexbuf}
    |'\n'
        {Lexing.new_line lexbuf; token_rule lexbuf}
    | "/*" {eat_rows lexbuf; token_rule lexbuf}
    | '(' {LParen(meta lexbuf)}
    | ')' {RParen(meta lexbuf)}
    | '[' {LBracket(meta lexbuf)}
    | ']' {RBracket(meta lexbuf)}
    | '{' {LCurly(meta lexbuf)}
    | '}' {RCurly(meta lexbuf)}
    | ',' {Comma(meta lexbuf)}
    | '.' {Dot(meta lexbuf)}
    | ';' {SemiC(meta lexbuf)}
    | '!' {Not(meta lexbuf)}
    | '~' {BinaryNot(meta lexbuf)}
    | '+' {Plus(meta lexbuf)}
    | '-' {Minus(meta lexbuf)}
    | '*' {Multiplication(meta lexbuf)}
    | '/' {Division(meta lexbuf)}
    | '%' {Modulo(meta lexbuf)}
    | "++" {Inc(meta lexbuf)}
    | "--" {Dec(meta lexbuf)}
    | '<' {LT(meta lexbuf)}
    | '>' {GT(meta lexbuf)}
    | "<=" {LEQ(meta lexbuf)}
    | ">=" {GEQ(meta lexbuf)}
    | "==" {Equality(meta lexbuf)}
    | "!=" {Inequality(meta lexbuf)}
    | '&' {BinaryAnd(meta lexbuf)}
    | '|' {BinaryOr(meta lexbuf)}
    | "&&" {LogicalAnd(meta lexbuf)}
    | "||" {LogicalOr(meta lexbuf)}
    | "<<" {ShiftLeft(meta lexbuf)}
    | ">>" {ShiftRight(meta lexbuf)}
    | '=' {Assign(meta lexbuf)}
    | "void" {Void(meta lexbuf)}
    | "int" {TypeInt(meta lexbuf)}
    | "char" {TypeChar(meta lexbuf)}
    | "struct" {TypeStruct(meta lexbuf)}
    | "for" {For(meta lexbuf)}
    | "while" {While(meta lexbuf)}
    | "if" {If(meta lexbuf)}
    | "else" {Else(meta lexbuf)}
    | "break" {Break(meta lexbuf)}
    | "return" {Return(meta lexbuf)}
    | "delete" {Delete(meta lexbuf)}
    | "extern" {Extern(meta lexbuf)}
    | "new" {New(meta lexbuf)}
    | ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']* as lxm
        {Identifier(meta lexbuf, lxm)}
    | '\'' (([' ' '!' '#' - '&' '('- '[' ']'-'~' ] | "\\n" | "\\t" | "\\\\" | "\\\'" | "\\\"" ) as lxm) '\''
        {CharConstant(meta lexbuf,lxm)}
    | "\"" (([' ' '!' '#' - '&' '('- '[' ']'-'~' ] | "\\n" | "\\t" | "\\\\" | "\\\'" | "\\\"" )* as lxm) "\"" 
        {StringConstant(meta lexbuf,lxm)}
    | '0' | ['1'-'9'] ['0'-'9']* | '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+ as lxm
        {IntConstant(meta lexbuf, int_of_string lxm)}
    | "0x" ('0' | ['1'-'9'] ['0'-'9']* | '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+) as lxm
        {IntConstant(meta lexbuf, int_of_string lxm)}
    | _ as b {ErrorLex(meta lexbuf,"Encountered unexpected string: " ^(String.make 1 b))}
    | eof
        {EOF(meta lexbuf)}
    | _ as c {ErrorLex(meta lexbuf, String.make 1 c)}

and eat_rows = parse
    "*/" {()}
    | '\n' { Lexing.new_line lexbuf; eat_rows lexbuf }
    | _ {eat_rows lexbuf}