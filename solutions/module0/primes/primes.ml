(* To parse command-line arguments, checkout OCamls `Arg` module
   (https://ocaml.org/manual/5.1/api/Arg.html). To control the exit code on
   errors you can use `Arg.parse_argv` instead of `Arg.parse` *)
open Printf
open Primeprint

let main =
   let len = Array.length Sys.argv in 
   if len <> 2 then 
      exit 2 
   else
      let n = 
      try int_of_string (Sys.argv.(1)) with
      | Failure(_) -> exit 2
      in 
      print_prime n
         