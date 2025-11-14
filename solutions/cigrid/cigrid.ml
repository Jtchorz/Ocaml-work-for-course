open Printf
open Ast 
open Parser
open Namecheck
open Typecheck
(*comment just to grade, XD*)

let usage_msg = "cigrid [--pretty-print] <filename>"
let pretty_print = ref false 
let pretty_tok = ref false
let name_analysis = ref false
let precise_error = ref false
let type_check = ref false
let input_file = ref ""

let current = ref 0
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

let rec prettyprint lexbuf = 
   match (Lexer.token lexbuf) with
   | Eof -> exit 0
   | _ as tok -> printf "%s\n" (printtok tok); prettyprint lexbuf

let rec prettytok filename = 
   let lexbuf = Lexing.from_channel (open_in filename) in 
   try ignore(prettyprint lexbuf) with
   | Lexer.Error(c) -> printf "Error, unexpected character at line %d, the character is %c \n" lexbuf.lex_curr_p.pos_lnum c; exit 1
   | Lexer.UnterminatedComment(n) -> printf "Error a comment run away. The comment starts at line %d \n" n; exit 1

   
let parse filename = 
   let lexbuf = Lexing.from_channel (open_in filename) in 
   let res =
      try Parser.main Lexer.token lexbuf with
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

      
let speclist =
       [("--pretty-print", Arg.Set pretty_print, "Pretty print ast");
       ("--pretty-tok", Arg.Set pretty_tok, "Pretty print all tokens");
       ("--line-error", Arg.Set precise_error, "only print line number on errors");
      ("--name-analysis", Arg.Set name_analysis, "analyze variable names in fucntion definitions");
      ("--type-check", Arg.Set type_check, "simple type analysis")]
let anon_fun f =
   input_file := f

let () =
   try(
   Arg.parse_argv ~current Sys.argv speclist anon_fun usage_msg;
   if !pretty_tok then (
      prettytok !input_file; 
      exit 0
   ) else (
      let ast = parse !input_file in
      if !pretty_print then (printf "%s" (pprint_program ast));
      if !name_analysis then (name_check_program ast);
      if !type_check then (type_check_program ast);
      exit 0
   ))
   with 
   | Arg.Help msg -> printf "%s\n" msg; exit 0
   | Arg.Bad msg -> printf "%s\n" msg; exit 1
   | DoubleDecl (s, n) -> 
      printf "Error, double decl of %s\n" s;
      eprintf "%d\n" n; 
   | UndeclaredVariable (s,n) -> 
      printf "Error, undeclared variable %s\n" s; 
      eprintf "%d\n" n;
   | UndeclaredFunction (s,n) -> 
      printf "Error, undeclared function %s\n" s; 
      eprintf "%d\n" n; 
   | UndeclaredStruct (s,n) -> 
      printf "Error, undeclared struct %s\n" s; 
      eprintf "%d\n" n;  
   | TypeMismatch (s,n) -> 
      printf "Error, type mismatch of %s\n" s; 
      eprintf "%d\n" n;
   | IndexTypeMismatch (s,n) -> 
      printf "Error, Index is not int %s\n" s; 
      eprintf "%d\n" n;
   | ConditionMismatch (n) ->
      printf "Error, Condition is type void \n"; 
      eprintf "%d\n" n;
   | IllegalPointerOperation (n) ->
      printf "Error, Illegal pointer operation \n"; 
      eprintf "%d\n" n;
   exit 2