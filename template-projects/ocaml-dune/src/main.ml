(* To parse command-line arguments, checkout OCamls `Arg` module
   (https://ocaml.org/manual/5.1/api/Arg.html). To control the exit code on
   errors you can use `Arg.parse_argv` instead of `Arg.parse` *)

open Ast

let pprint = function
  | EHello  w -> "Hello, " ^ w ^ "!"

let _ =
  try
    let lexbuf = Lexing.from_string "Hello, World!" in
    let expr = Parser.expr Lexer.token lexbuf in
    print_endline (pprint expr)
  with _ -> exit 1
