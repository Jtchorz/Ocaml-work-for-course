{
    open Parser
    exception Error of char
    exception UnterminatedComment of int
    let commentBegin = ref 0
}

let line_comment = ("//" | '#') [^ '\n']*
 
let mline_comment = "/*" "*/"

let ident = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let number = ('0'|['1'-'9']['0'-'9']*) | ('0'['x' 'X']['0'-'9' 'a'-'f' 'A'-'F']+)

let character = [^ '\\' '"'] | ('\\'['n''t''\\''\'''\"'])
let s_character = [^ '\\' '\"'] | ('\\'['n''t''\\''\'''\"'])

rule token = parse
    | line_comment
        { token lexbuf }
    | "/*"
        { commentBegin := lexbuf.lex_curr_p.pos_lnum; comment lexbuf; token lexbuf  }
    | [' ' '\t' '\r']
        { token lexbuf }
    | '\n'
        { Lexing.new_line lexbuf; token lexbuf }
    | ident as str 
        { match str with
            | "break" -> Break
            | "char" -> Char
            | "delete" -> Delete
            | "else" -> Else
            | "extern" -> Extern
            | "for" -> For
            | "if" -> If
            | "int" -> Int
            | "new" -> New
            | "return" -> Return
            | "struct" -> Struct 
            | "void" -> Void
            | "while" -> While
            | s -> Ident(s)
        }
    | number as n
        { IntConst(int_of_string n) }
    | '\'' (character as c) '\''  
        { match c with 
            | "\\n" -> CharConst('\n')
            | "\\t" -> CharConst('\t')
            | "\\\\" -> CharConst('\\')
            | "\\\'" -> CharConst('\'')
            | "\\\"" -> CharConst('\"')
            | _ -> CharConst(c.[0])
         }
    | '"' (s_character* as s) '"'
        { StringConst(s)}
    | "<<"
        { ShiftLeft }
    | ">>"
        { ShiftRight }
    | "<="
        { SmallerEqual }
    | ">="
        { BiggerEqual }
    | "=="
        { DoubleEqual }
    | "!="
        { NotEqual }
    | "&&"
        { AndAnd }
    | "||"
        { Or }
    | "++"
        { PlusPlus }
    | "--"
        { MinusMinus }
    | ['+']
        { Plus }
    | ['-']
        { Minus }
    | ['~']
        { Tylde }
    | ['!']
        { Exclamation }
    | ['*']
        { Star }
    | ['/']
        { FSlash }
    | ['|']
        { Bar }
    | ['%']
        { Percent }
    | ['&']
        { And }
    | [';']
        { SemiColon }
    | ['(']
        { LParent }
    | [')']
        { RParent }
    | ['<']
        { SmallerThan }
    | ['>']
        { BiggerThan }
    | ['=']
        { Equal }
    | ['{']
        { LCurly }
    | ['}']
        { RCurly }
    | [' ' '\t' '\r']
        { token lexbuf }
    | '\n'
        { Lexing.new_line lexbuf; token lexbuf }   
    | [',']
        { Comma }
    | ['[']
        { RSquare }
    | [']']
        { LSquare }
    | ['.']
        { Dot }
    | eof
        { Eof }
    | _ as c
        { raise (Error(c))}
and comment = parse
    | "*/"
        { () }
    | '\n' 
        { Lexing.new_line lexbuf; comment lexbuf }
    | eof 
        { raise (UnterminatedComment !commentBegin)}
    | _ 
        { comment lexbuf }