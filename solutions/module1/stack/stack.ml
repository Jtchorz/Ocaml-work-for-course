open Printf
open Lexeri

type expr =
   | PushE of int
   | PopE
   | AddE
   | SubE
   | MulE
   | DivE
   | ShowE 
   | Endprogram

type exprs =
   | Many of expr * exprs
   | One of expr
let prettytok t = 
   match t with
      | Num n -> printf ("Num %d \n") n
      | Push -> printf ("Push\n")
      | Semi -> printf ("Semi\n") 
      | Pop -> printf ("Pop\n")
      | Add -> printf ("Add\n") 
      | Sub -> printf ("Sub\n") 
      | Mul -> printf ("Mul\n") 
      | Div -> printf ("Div\n")
      | Show -> printf ("Show\n")
      | Endline -> printf ("Endline\n")
      | Eof -> printf ("Eof\n")

let prettyexpr t = 
   match t with
      | PushE(n) -> printf ("Push %d \n") n
      | PopE -> printf ("Pop\n")
      | AddE -> printf ("Add\n") 
      | SubE -> printf ("Sub\n") 
      | MulE -> printf ("Mul\n") 
      | DivE -> printf ("Div\n")
      | ShowE -> printf ("Show\n")
      | Endprogram -> printf "you've reached the end of the program \n"
let parseExpr t = 
   match t with 
      | Push -> (let nt = nextToken () in match nt with
         | Num num -> (let nt2 = nextToken () in match nt2 with
            | Semi -> (nextToken (), PushE(num))
            | _ -> printf "error missing semicolon\n"; exit 1
            )
         | _ -> printf "not a number \n"; exit 1
         )
      | Pop -> (match nextToken () with
            | Semi -> (nextToken (), PopE)
            | _ -> printf "error missing semicolon\n"; exit 1
            )
      | Add -> (match nextToken () with
            | Semi -> (nextToken (), AddE)
            | _ -> printf "error missing semicolon\n"; exit 1
            )
      | Sub -> (match nextToken () with
            | Semi -> (nextToken (), SubE)
            | _ -> printf "error missing semicolon\n"; exit 1
            )
      | Mul -> (match nextToken () with
            | Semi -> (nextToken (), MulE)
            | _ -> printf "error missing semicolon\n"; exit 1
            )
      | Div -> (match nextToken () with
            | Semi -> (nextToken (), DivE)
            | _ -> printf "error missing semicolon\n"; exit 1
            )
      | Show -> (match nextToken () with
            | Semi -> (nextToken (), ShowE)
            | _ -> printf "error missing semicolon\n"; exit 1
            )
      | Endline | Eof -> (Endline, Endprogram)
      | _ -> printf "error, expected a command token \n"; exit 1

let rec parseExprs t =
   let (nt, exp) = parseExpr t in match nt with
   | Eof | Endline -> (nt, One(exp)) (*only will check these as parseExpr can handle not getting a word token, no reason to double up err handling*)
   | _ -> let (nt2, exps) = parseExprs nt in (nt2, Many(exp, exps))

let rec prettyast e = 
   match e with
   | One(exp) -> prettyexpr exp
   | Many(exp, exps) -> prettyexpr exp; prettyast exps

let execute e stack= 
   match e with
   | PushE(n) -> n :: stack
   | PopE -> (match stack with
      | [] -> printf "error, nothing to pop \n"; exit 2
      | _::nstack -> nstack
      )
   | AddE -> (match stack with
      | a::b::nstack -> (a+b)::b::nstack
      | [] | [_] -> printf "error, not enough elements to execute operation \n"; exit 2

      )
   | SubE -> (match stack with
      | a::b::nstack -> (a-b)::b::nstack
      | [] | [_] -> printf "error, not enough elements to execute operation \n"; exit 2
      )
   | MulE -> (match stack with
      | a::b::nstack -> (a*b)::b::nstack
      | [] | [_] -> printf "error, not enough elements to execute operation \n"; exit 2
      )
   | DivE -> (match stack with
      | a::b::nstack -> (a/b)::b::nstack
      | [] | [_] -> printf "error, not enough elements to execute operation \n"; exit 2
      )
   | ShowE -> (match stack with
      | [] -> printf "stack is empty \n"; stack
      | a::nstack -> printf "%d\n" a; a::nstack
      )
   | Endprogram -> exit 0


let rec executeall e stack =
   match e with
   | One(exp) -> ignore(execute exp stack); ()
   | Many(exp, exps) -> executeall exps (execute exp stack)

let rec main () =
   let next = nextToken() in 
   let (t, e) = parseExprs next in match t with
   | Eof -> executeall e []
   | Endline -> executeall e []; flush stdout; main() (*printf "Ast constructed: \n"; prettyast e; exit 0*)
   | _ -> printf "Unexpected parsing error"; exit 1

let () = main ()