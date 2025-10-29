(* To parse command-line arguments, checkout OCamls `Arg` module
   (https://ocaml.org/manual/5.1/api/Arg.html). To control the exit code on
   errors you can use `Arg.parse_argv` instead of `Arg.parse` *)
open Printf
open Primegen

let primes n = prime_list n

let pretty lst = 
   let rec loop lst =
      match lst with
      | [] -> ""
      | [x] -> string_of_int x
      | x :: l -> string_of_int x ^ "," ^ loop l
   in 
      loop lst



let main =
   let len = Array.length Sys.argv in 
   if len <> 2 then 
      exit 2 
   else
      let n = 
      try int_of_string (Sys.argv.(1)) with
      | Failure(_) -> exit 2
      in 
      printf "%s\n" (pretty(primes n))
         