{
  open Parser
  exception Error
}

let white = [' ' '\t' '\n']+
let word = ['a'-'z' 'A'-'Z']+

rule token = parse
     white { token lexbuf }
   | "Hello" { HELLO }
   | '!' { EXCLAM }
   | ',' { COMMA }
   | word as w { WORD w }
   | eof { EOF }
   | _ { raise Error }