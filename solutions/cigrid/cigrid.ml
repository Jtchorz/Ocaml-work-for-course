open Printf
open Parser
open Lexer
let printtok tok =
   match tok with
  | Break -> "Break"
  | Char -> "Char"
  | Delete -> "Delete"
  | Else -> "Else"
  | Extern -> "Extern"
  | For -> "For"
  | If -> "If"
  | Int -> "Int"
  | New -> "New"
  | Return -> "Return"
  | Struct -> "Struct"
  | Void -> "Void"

  | Ident s -> "Ident(" ^ s ^ ")"
  | IntConst n -> "IntConst(" ^ string_of_int n ^ ")"
  | CharConst c -> sprintf "CharConst(%c)" c
  | StringConst s -> "StringConst(" ^ s ^ ")"

  | Exclamation -> "!"
  | Tylde -> "~"
  | Minus -> "-"
  | Star -> "*"
  | FSlash -> "/"
  | Percent -> "%"
  | Plus -> "+"
  | SmallerThan -> "<"
  | BiggerThan -> ">"
  | Equal -> "="
  | And -> "&"
  | Bar -> "|"
  | RParent -> ")"
  | LParent -> "("
  | SemiColon -> ";"
  | RCurly -> "}"
  | LCurly -> "{"
  | Comma -> ","
  | RSquare -> "]"
  | LSquare -> "[]"
  | Dot -> "."

  | ShiftLeft -> "<<"
  | ShiftRight -> ">>"
  | SmallerEqual -> "<="
  | BiggerEqual -> ">="
  | DoubleEqual -> "=="
  | NotEqual -> "!="
  | AndAnd -> "&&"
  | Or -> "||"

  | Eof -> "EOF"

let rec prettyprint lexbuf = 
   match (token lexbuf) with
   | Eof -> exit 0
   | _ as tok -> printf "%s\n" (printtok tok); prettyprint lexbuf
let main = 
   let lexbuf = Lexing.from_channel stdin in 
   try prettyprint lexbuf with
   | Error(c) -> printf "Error, unexpected character at line %d, the character is %c \n" lexbuf.lex_curr_p.pos_lnum c
   | UnterminatedComment(n) -> printf "Error a comment run away. The comment starts at line %d \n" n
         