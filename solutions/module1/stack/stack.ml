(* To parse command-line arguments, checkout OCamls `Arg` module
   (https://ocaml.org/manual/5.1/api/Arg.html). To control the exit code on
   errors you can use `Arg.parse_argv` instead of `Arg.parse` *)
open Printf
open Lexeri

type tree =
  | Node of string * tree * tree
  | Leaf of int


let prettys lst = 
   let rec loop lst =
      match lst with
      | [] -> ""
      | [x] -> (*string_of_int*) x
      | x :: l -> (*string_of_int*) x ^ "\n" ^ loop l
   in 
      loop lst

let prettyi lst = 
   let rec loop lst =
      match lst with
      | [] -> ""
      | [x] -> string_of_int x
      | x :: l -> string_of_int x ^ "," ^ loop l
   in 
      loop lst

let main =
     let te = nextToken() in let rec work t =
      match t with
         | Num n -> printf ("Num %d \n") n; work (nextToken())
         | Push -> printf ("push\n"); work (nextToken())
         | Semi -> printf ("Semi\n"); work (nextToken())
         | Pop -> printf ("Pop\n"); work (nextToken())
         | Add -> printf ("Add\n"); work (nextToken())
         | Sub -> printf ("Sub\n"); work (nextToken())
         | Mul -> printf ("Mul\n"); work (nextToken())
         | Div -> printf ("Div\n"); work (nextToken())
         | Show -> printf ("Show\n"); work (nextToken())
         | Endline -> printf ("Endline\n%!"); work (nextToken())
         | Eof -> printf ("Eof\n%!");
     in work te
      
