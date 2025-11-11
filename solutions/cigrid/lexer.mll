{
    open Parser
    exception Error of char
    exception UnterminatedComment of int
    let commentBegin = ref 0
}

let line_comment = "//" [^ '\n']*
let mline_comment = "/*" "*/"

let ident = [_a-zA-Z][_a-zA-Z0-9]*
let number = (0|[1-9][0-9]*) | (0[xX][0-9a-fA-F]+)

let character = [^ '\\' '"'] | ('\\'['n''t''\\''\'''\"'])
let s_character = [^ '\\' '\"'] | ('\\'['n''t''\\''\'''\"'])

rule token = parse
    | line_comment
        { token lexbuf }
    | "/*"
        { commentBegin := Lexing.pos_lnum; comment lexbuf; token lexbuf  }
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
    | eof
        { Eof }

and comment = parse
    | "*/"
        { () }
    | '\n' 
        { Lexing.new_line lexbuf; comment lexbuf }
    | eof 
        { raise (UnterminatedComment !commentBegin)}
    | _ 
        { comment lexbuf }