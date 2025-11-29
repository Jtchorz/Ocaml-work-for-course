open Printf
open Ast
open Parser
open Pprinter
open Semantics
open Cfg
open Asm



type parse_result = 
  | ParseSuccess of program
  | ParseFailure of int

let get_token_list lexbuf =
  let rec work acc =
    match Lexer.token_rule lexbuf with
    | EOF(x) -> (List.rev (EOF(x)::acc))
    | t -> work (t::acc)
  in work []

let generate_ast fn= 
  let lexbuf2 = Lexing.from_channel (In_channel.open_bin fn) in 
  try
    ParseSuccess(Parser.init Lexer.token_rule lexbuf2)
  with
  | _-> ParseFailure(lexbuf2.lex_curr_p.pos_lnum)


let bool_of_opt = function 
  |None -> false
  |Some(_)->true

let check_simple_name_analysis tree line_error= 
  match simple_name_analysis tree with 
      | AnalysisSuccess ->  ()
      | AnalysisNameError(n) -> (if line_error then fprintf stderr "%d\n" n else ()); exit 2
      | AnalysisTypeError(n) -> (if line_error then fprintf stderr "%d\n" n else ()); exit 2 (*Never happens*)

let check_type_check tree line_error= 
  match static_semantics tree with 
      | AnalysisSuccess -> ()
      | AnalysisNameError(n) -> (if line_error then fprintf stderr "%d\n" n else ()); exit 2
      | AnalysisTypeError(n) -> (if line_error then fprintf stderr "%d\n" n else ()); exit 2

let main = 
  let arg_len = Array.length Sys.argv in 
  if arg_len < 2 then exit 1 else
  let input_file = Array.get Sys.argv (arg_len-1) in
  let pprint = Array.find_index (fun x -> x = "--pretty-print") Sys.argv |> bool_of_opt in 
  let line_error = Array.find_index (fun x -> x = "--line-error") Sys.argv |> bool_of_opt in 
  let name_analysis = Array.find_index (fun x -> x = "--name-analysis") Sys.argv |> bool_of_opt in 
  let type_check = Array.find_index (fun x -> x = "--type-check") Sys.argv |> bool_of_opt in 
  let test_flag = Array.find_index (fun x -> x = "--test") Sys.argv |> bool_of_opt in
  let asm = Array.find_index (fun x -> x = "--asm") Sys.argv |> bool_of_opt in
  if test_flag then print_endline ( get_token_list (Lexing.from_channel (In_channel.open_bin input_file)) |> string_of_tokens ) else 
  let num_flags = List.fold_left (+) 0 (List.map Bool.to_int [pprint; line_error;name_analysis; type_check;asm]) in
  if num_flags + 2 <> arg_len then exit 1 else 
  match generate_ast input_file with 
  | ParseFailure(line) -> (fprintf stderr "%d\n" line; exit 1)
  | ParseSuccess(tree) -> (
    (if name_analysis then check_simple_name_analysis tree line_error else ());
    (if type_check then check_type_check tree line_error else ());
    (if pprint then print_endline (pprint_program tree) else ());
    (if asm then (
      let (cfg, cfg_env) = ast_to_cfg tree in 
    (* print_endline (pprint_ir_cfg cfg) ;  *)
      let (asm_funcs, asm_env) = cfg_to_asm cfg cfg_env in 
      print_endline (pprint_assembly asm_funcs cfg_env.extern_funcs asm_env.string_labels asm_env.global_vars_labels)
    ) else ())
  )