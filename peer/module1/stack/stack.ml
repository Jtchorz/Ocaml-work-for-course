open Printf

exception ReadError 		of string
exception EOF	     		of string
exception StackError 		of string
exception UnknownTokenError 	of string
exception ParseError		of string

type token = 
	| TokPush
	| TokNum of int
	| TokPop 	
	| TokAdd 	
	| TokSub 	
	| TokMul 	
	| TokDiv 	
	| TokShow 	
	| TokEOL
	| TokEOF
	| SEMICOLON

let pprint_token = function
	| TokPush -> "push"
	| TokNum(n) -> string_of_int n
	| TokPop -> "pop"
	| TokAdd -> "add"
	| TokSub -> "sub"
	| TokMul -> "mul"
	| TokDiv -> "div"
	| TokShow -> "show"
	| TokEOL -> "eol"
	| TokEOF -> "eof"
	| SEMICOLON -> ";" 

let is_char_num c =
	if c >= '0' && c <= '9' then true
	else false

(* is_str_num():
 * 	Uses String.for_all to check that all chars in string are digits.
 *	If it at some point fails then entire for_all fails.
 *	String.for_all return true for string with len 0.
 *)
let is_num str =
	if (String.length str) = 0 then false
	else (
		(* Printf.printf "is_num: '%s'\n" str; *)
		String.for_all is_char_num str
	)



(* =========================================================== *)
(* LEXER/SCANNER *)
(* =========================================================== *)
(* readChar():
 *	 Read continously through file descriptor (channel) 
 *)
let readChar () =
	try 
		input_char stdin
	with
		| End_of_file -> raise (EOF "End of file reached.")
		| Failure(msg) -> raise (ReadError "failure when reading char.")


(* readNum():
 *	Recursively continue reading characters as long as new char is a number.
 *	When new char is no longer a number then we need to move file pointer back
 *	1 step and return the number.
 *)
let rec readNum num_str = 
	let s = String.make 1 (readChar ()) in
	if is_num s then
		readNum  (num_str ^ s)
	else (
		Stdlib.seek_in stdin ((Stdlib.pos_in stdin) - 1);
		num_str
	)

(* readToken():
 * 	Recursively compare current string with any possible tokens.
 *	If " " or "\t" then ignore and reset to "".
 *	cases like "push  " should never happen because then it already faulty.
 *	If char is number then go to readNum() which reads entire number.
 *	If no match then as long as acc_str < 4 then continue building.
 *	Otherwise no possible match and raise error.
 *)
let rec readToken acc_str =
	try
		match acc_str with
		| "push" 	-> TokPush
		| "pop" 	-> TokPop 	
		| "add"		-> TokAdd 	
		| "sub"		-> TokSub 	
		| "mul"		-> TokMul 	
		| "div"		-> TokDiv 	
		| "show"	-> TokShow 	
		| "\n"		-> TokEOL
		| ";"		-> SEMICOLON
		| " "		-> 
					let s = String.make 1 (readChar ()) in
					readToken s
		| "\t"		-> 
					let s = String.make 1 (readChar ()) in
					readToken s

		| _ as c	->
					if is_num c then (
						(* Printf.printf "c: '%s' len: %d\n" c (String.length c); *)
						TokNum(int_of_string (readNum c))
					)
					else
						if (String.length acc_str) > 4 then
							let msg = Printf.sprintf "unknown = '%s'\n" c in
							raise (UnknownTokenError msg)
						else
							let s = String.make 1 (readChar ()) in
							readToken (acc_str ^ s)
						
	with
	| EOF(_) -> 
			if (String.length (String.trim acc_str)) = 0 then TokEOF
			else raise (UnknownTokenError "EOF reached while building token.")
	| Failure(msg) -> raise (UnknownTokenError msg)

(* getToken():
 *	collects 1 token by calling readToken().
 *)
let getToken () = 
	let tok = readToken "" in
	(* Printf.printf "getToken() : '%s'\n" (pprint_token tok); *)
	(tok)



(* =========================================================== *)
(* PARSER *)
(* =========================================================== *)

(* parseExpr():
 * 	TokPush then we know that <next> token will be 
 *	a number TokNum(n) and we add this to top of stack list.
 *
 *	TokPop then we remove top of stack. 
 *	If stack is empty then just return empty stack.
 *
 * 	TokAdd picks out <top stack> + <2nd top stack> and
 *	places result in top of stack.
 *
 * 	TokSub picks out <top stack> - <2nd top stack> and
 *	places result in top of stack.
 *
 * 	TokMul picks out <top stack> * <2nd top stack> and
 *	places result in top of stack.

 * 	TokDiv picks out <top stack> / <2nd top stack> and
 *	places result in top of stack.
 *
 * 	TokShow prints out the top item of stack.
 * 
 *	TokEOL (should only reach this case if next tok is EOF?)
 *	TokEOF (should never be reached here?)
 *)
let parseExpr token st =
	let next = getToken ()
	in
	match token with
	| TokPush -> (
		let next2 = getToken ()
		in
		match next with
		| TokNum n -> (next2, (n :: st))
		| _	 -> raise (ParseError "parseExpr token after push is not number.")
	)
	| TokPop -> (
		match st with
		| hd :: rs -> (next, rs)	
		| []	   -> (next, [])
	)
	| TokAdd -> (
		match st with
		| a :: b :: rs ->
			let sum = a + b
			in
			(next, (sum :: rs))
		| a :: [] -> raise (StackError "only 1 item in stack.")
		| [] 	  -> raise (StackError "add: stack is empty")
	)
	| TokSub -> (
		match st with
		| a :: b :: rs ->
			let dif = a - b
			in
			(next, (dif :: rs))
		| a :: [] -> raise (StackError "only 1 item in stack.")
		| [] 	  -> raise (StackError "sub: stack is empty")
	)
	| TokMul -> (
		match st with
		| a :: b :: rs ->
			let fac = a * b
			in
			(next, (fac :: rs))
		| a :: [] -> raise (StackError "only 1 item in stack.")
		| [] 	  -> raise (StackError "mul: stack is empty")
	)
	| TokDiv -> (
		match st with
		| a :: b :: rs ->
			let q = a / b
			in
			(next, (q :: rs))
		| a :: [] -> raise (StackError "only 1 item in stack.")
		| [] 	  -> raise (StackError "div: stack is empty")
	)
	| TokShow -> (
		match st with
		| hd :: rs -> 
			print_endline (string_of_int hd);
			(next, st)
		| [] -> 
			print_endline "stack is empty";
			(next, st)
	
	)
	| TokEOL -> (next, st)
	| TokEOF -> (next, st)
	| _	 -> raise (ParseError "parseExpr match not found.")

	

(* parseExprs():
 *	Calls parseExpr() to deduce token and update stack
 *	parseExpr() returns "PUSH NUM", "POP", "SHOW", etc.
 *	and to finish expression we thus always expect ";" (semicolon) here.
 *
 *	If tok is ";" then continue on onto next expression otherwise throw error
 *	if next = EOL or EOF then return and go back to parseProgram()
 *)
let rec parseExprs token st = 
	let (tok, stk) = parseExpr token st
	in
	(* Printf.printf "parseExprs(): (tok: %s, stk)\n" (pprint_token tok); *)
	if tok = SEMICOLON then
		let next = getToken ()
		in
		(* Printf.printf "parseExprs(): (next = '%s')\n" (pprint_token next); *)
		if (next = TokEOL || next = TokEOF) then (next, stk)
		else parseExprs next stk
	else raise(ParseError "No semicolon ';' after statement.")

(* parseProgram():
 * 	Calls parseExprs() for parsing of each line of statements.
 * 	When parseExprs() returns we except either EOF or EOL.
 * 	If EOL then check that next = EOL so "\n\n" meaning exit.
 *
 * 	Should we only get 1 EOL then continue safely.
 * 	If EOF then return and exit directly.
 * 	Any other token here means error occured.
 *)
let rec parseProgram token st = 
	let (tok, stk) = parseExprs token st in
	(* Printf.printf "parseProgram(): (tok: %s, stk)\n" (pprint_token tok); *)
	if tok = TokEOF then
		(tok, stk)
	else
		if tok = TokEOL then
			let next = getToken () in
			if next = TokEOL || next = TokEOF then
				(next, stk)
			else 
				 parseProgram next []
		else
			raise (ParseError "EachLine tok is not EOF or EOL.")

(* parseMain():
 *	Start function for recursion.
 *	Initiates empty stack.
 *)
let parseMain () = 
	let first = getToken () in
	if first = TokEOF then (first, [])
	else parseProgram first []

(* Main(): 
 *	Exceptions:
 *	Failure -> unknown runtime error means 		exit 2
 *	ReadError -> error file reader in lexer 	exit 1
 *	UnknownTokenError -> error in lexer 		exit 1
 *)
let main =                                                                        
	try 
		parseMain ();
		(* Printf.printf "exit 0\n"; *)
		exit 0
	with
	| Failure(msg) 			-> print_endline("Failure Error: " ^ msg); exit 2
	| ReadError(msg)		-> print_endline(msg); exit 1
	| UnknownTokenError(msg)	-> print_endline(msg); exit 1
	| ParseError(msg)		-> print_endline(msg); exit 1
