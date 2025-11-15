open Parser
open Printf
let tok_to_string tok =
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
  | While -> "While"

  | Ident(s) -> "Ident(" ^ s ^ ")"
  | IntConst(n) -> "IntConst(" ^ string_of_int n ^ ")"
  | CharConst(c) -> sprintf "CharConst(%c)" c
  | StringConst(s) -> "StringConst(" ^ s ^ ")"

  | PlusPlus -> "++"
  | MinusMinus -> "--"
  | ShiftLeft -> "<<"
  | ShiftRight -> ">>"
  | SmallerEqual -> "<="
  | BiggerEqual -> ">="
  | DoubleEqual -> "=="
  | NotEqual -> "!="
  | AndAnd -> "&&"
  | Or -> "||"

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
  | LSquare -> "["
  | Dot -> "."
  | Eof -> "EOF"


let rec print_all lexbuf = 
   match (Lexer.token lexbuf) with
   | Eof -> exit 0 
   | _ as tok -> printf "%s\n" (tok_to_string tok); print_all lexbuf

let rec print_tokens filename = 
   let lexbuf = Lexing.from_channel (open_in filename) in 
   try ignore(print_all lexbuf) with 
   | Lexer.Error(c) -> printf "Error, unexpected character at line %d, the character is %c \n" lexbuf.lex_curr_p.pos_lnum c; exit 1
   | Lexer.UnterminatedComment(n) -> printf "Error a comment run away. The comment starts at line %d \n" n; exit 1

   (*end of lexer section, start parser section
   ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
   *)

(*I am not using the ln from ast as it is not yet added to the node at which something failed*)
let parse filename precise_error = 
   let lexbuf = Lexing.from_channel (open_in filename) in 
   let res = try Parser.main Lexer.token lexbuf with
   | Lexer.Error(c) -> 
      if !precise_error then (eprintf "%d\n" lexbuf.lex_curr_p.pos_lnum);
      printf "Error, unexpected character at line %d, the character is %c \n" lexbuf.lex_curr_p.pos_lnum c; 
      exit 1 
   | Lexer.UnterminatedComment(n) -> 
      if !precise_error then (eprintf "%d\n" n);
      printf "Error a comment run away. The comment starts at line %d \n" n; 
      exit 1
   | Parser.Error -> 
      if !precise_error then (eprintf "%d\n" lexbuf.lex_curr_p.pos_lnum);
      printf "Unknown parse error at line %d\n" lexbuf.lex_curr_p.pos_lnum; 
      exit 1
   in res