open Printf
open Ast 
open Namecheck
open Typecheck
open IR
open IRgen
open AsmIr

let usage_msg = "cigrid [--pretty-print] <filename>"
let pretty_print = ref false 
let pretty_tok = ref false
let name_analysis = ref false
let precise_error = ref false
let type_check = ref false
let ir_print = ref false
let input_file = ref ""
let asm_print = ref false
let compile = ref false
let current = ref 0

let testasmIr = Func("main",
   [Block("main", ([BinOp(Add,Imm(1),Reg(1,QWord))],Ret))]
)
      
let speclist =
       [("--pretty-print", Arg.Set pretty_print, "Pretty print ast");
       ("--pretty-tok", Arg.Set pretty_tok, "Pretty print all tokens");
       ("--line-error", Arg.Set precise_error, "only print line number on errors");
      ("--name-analysis", Arg.Set name_analysis, "analyze variable names in fucntion definitions");
      ("--type-check", Arg.Set type_check, "simple type analysis");
      ("--ir", Arg.Set ir_print, "pretty-print the ir representation");
      ("--asm", Arg.Set asm_print, "pretty-print the assembly code");
      ("--compile", Arg.Set compile, "compile the produced assembly code");]
let anon_fun f =
   input_file := f

let () =
   (try
      Arg.parse_argv ~current Sys.argv speclist anon_fun usage_msg
   with
   | Arg.Help msg -> printf "%s\n" msg; exit 0
   | Arg.Bad msg -> printf "%s\n" msg; exit 1
   );

   if !pretty_tok then ( Helper.print_tokens !input_file);
   
   let ast = Helper.parse !input_file precise_error in
   if !pretty_print then (printf "%s" (pprint_program ast));
   if !name_analysis then (name_check_program ast);
   if !type_check then (check_type ast);

   let (ir,beforeS) =(try convert_AST ast 
   with 
   | Failure(s) -> printf "%s" s; exit 0
   )in
   if !ir_print then ( printf "%s"  (pprint_ir_global ir));

   let asm = (try
   (InstrSelection.ir_global_to_asm ir)
   with 
   | Failure(s) -> (*printf "%s" s; *)exit 0
   | _ -> (*printf "idkwtf";*) exit 0
   )
   in
   let asm_string = (sprintf "%s\tglobal main \nsection .text\n%s" beforeS (pprint_func asm)) in
   if !asm_print then printf "%s" asm_string;
   if !compile then (
      let ch = open_out "a.asm" in 
      output_string ch asm_string;
      close_out ch;
      ignore(Sys.command "nasm -felf64 a.asm -o a.o");
      ignore(Sys.command "gcc -no-pie a.o");
      );
   exit 0
   
   
   