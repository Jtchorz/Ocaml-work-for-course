open Printf
open Ast

exception InvalidFlagError 	of string
exception InvalidFileError 	of string
exception LexerError 	  	of string 
exception ParserError		of string

let rec find_flag str f_lst =
	match f_lst with
	| [] -> false
	| hd :: rs -> 
		if (String.equal str hd) then true
		else (find_flag str rs)

let main f_lst file =
	let channel = Stdlib.open_in file in
		let lexbuf = Lexing.from_channel channel in
		let res =
			try
				Parser.program Lexer.token lexbuf 
			with
			(* If flag --line-error is enabled then raise error otherwise quietly exit. *)
			(* We only print out "num\n" so ignore 'c' here *)
			(* Lexer.Error(Some c) and Lexer.Error(None) are grouped together by (_) *)
			| Lexer.Error(_) ->
				if (find_flag "--line-error" f_lst) then
					let msg = Printf.sprintf "%d\n" lexbuf.lex_curr_p.pos_lnum in
					raise (LexerError msg)
				else 
					exit 1
			| Parser.Error -> 
				if (find_flag "--line-error" f_lst) then
					let msg = Printf.sprintf "%d\n" lexbuf.lex_curr_p.pos_lnum in
					raise (ParserError msg)
				else 
					exit 1
	in
	close_in channel;
	if find_flag "--pretty-print" f_lst then 
		Printf.printf "%s\n" (pprint_program res)
		
let rec cmd_line n acc =
	if n >= Array.length Sys.argv then (acc, None)
	else (
		let arg = Sys.argv.(n) in
		if (String.length arg >= 1 && arg.[0] = '-') then
			match arg with
			| "--pretty-print" 	-> cmd_line (n+1) ("--pretty-print" :: acc )	
			| "--line-error"	-> cmd_line (n+1) ("--line-error"   :: acc )
			| _ -> raise(InvalidFlagError "Invalid flag.")
		else
			(* Assume argument is a file. *)
			(acc, Some(arg))
	)

let init = 
	try 
		let (flags, filename) = cmd_line 1 [] in
		match filename with
		| None -> raise(InvalidFileError "No filename provided.")
		| Some(file) -> 
			try 
				main flags file;
				exit 0
			with
				| Sys_error(msg) -> raise(InvalidFileError "Invalid file provided.")
					
	with
	| InvalidFlagError(msg) -> Printf.fprintf stderr "%s\n### MENU FLAGS ###\n--pretty-print\n" msg; exit 1
	| InvalidFileError(msg) -> Printf.fprintf stderr "%s\n" msg; exit 1
	| LexerError(msg) 	-> Printf.fprintf stderr "%s\n" msg; exit 1 
	| ParserError(msg) 	-> Printf.fprintf stderr "%s\n" msg; exit 1 
