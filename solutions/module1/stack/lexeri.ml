open Printf
open Buffer
let char_cnt = ref 0
let line_cnt = ref 0
let currentChar = ref 'a'
let atEOF = ref false
let nextChar () = 
  try (let c = input_char stdin in 
  currentChar := c;
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
    | 'a' .. 'z' -> Buffer.add_char b !currentChar; nextChar(); work b
    | _ -> b
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
  | _ -> exit 1
let rec nextToken () =
  nextChar();
  if (!atEOF) then
    Eof
  else(
  match !currentChar with
  | '\n' -> incr line_cnt; char_cnt := 0; Endline
  | ';' -> Semi
  | ' ' | '\t' -> nextToken ()
  | 'p' | 'a' | 's' | 'm' | 'd' -> getcomm ()
  | '0' .. '9' as n -> Num (int_of_char n)
  | _ -> (printf "Unrecognized char at line %d char %d \n" !line_cnt !char_cnt; exit 1)
  )