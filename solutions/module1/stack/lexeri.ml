open Printf
open Buffer
let char_cnt = ref 0
let line_cnt = ref 0
let currentChar : char option ref = ref None
let atEOF = ref false
let nextChar () = 
  try (let c = input_char stdin in 
  currentChar := Some c;
  incr char_cnt)
  with
  |End_of_file -> atEOF := true

type token =
  | Num of int
  | Push
  | Semi 
  | Pop
  | Add
  | Sub 
  | Mul 
  | Div 
  | Show
  | Endline
  | Eof

let rec gets () = 
  let b = Buffer.create 4 in
  let rec work b = 
    match !currentChar with 
    | Some c -> (match c with
      | 'a' .. 'z' -> Buffer.add_char b c; nextChar(); work b
      | _ -> b)
    | None -> printf "very unexpected error during lexing at line %d char end at %d \n" !line_cnt !char_cnt ; exit 1
  in work b

let getcomm () =
  let s = Buffer.contents(gets () ) in
  match s with
  | "push" -> Push
  | "pop" -> Pop
  | "add" -> Add
  | "sub" -> Sub
  | "mul" -> Mul
  | "div" -> Div
  | "show" -> Show
  | _ -> printf "not a valid command at line %d char end at %d \n" !line_cnt !char_cnt ; exit 1

let rec getn () = 
  let b = Buffer.create 10 in
  let rec work b = 
    match !currentChar with 
    | Some c -> (match c with
      | '0' .. '9' -> Buffer.add_char b c; nextChar(); work b
      | _ -> b)
    | None -> printf "very unexpected error during lexing at line %d char end at %d \n" !line_cnt !char_cnt ; exit 1
  in work b

let getnum () =
  let s = Buffer.contents(getn () ) in
  try Num(int_of_string s) with
  | Failure _ -> printf "not a valid number at line %d ending at character %d \n" !line_cnt !char_cnt; exit 1


let rec nextToken () =
    if (!atEOF) then
      Eof
    else(
    match !currentChar with
    | Some c -> (match c with
      | '\n' -> incr line_cnt; char_cnt := 0; currentChar := None; Endline
      | ';' -> nextChar(); Semi
      | ' ' | '\t' -> nextChar(); nextToken ()
      | 'p' | 'a' | 's' | 'm' | 'd' -> getcomm ()
      | '0' .. '9' -> getnum ()
      | _ -> (printf "Unrecognized char at line %d char %d \n" !line_cnt !char_cnt; exit 1)
      )
    | None -> nextChar(); decr char_cnt; nextToken()
  )