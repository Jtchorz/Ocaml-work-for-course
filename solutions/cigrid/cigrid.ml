open Printf
open Ast 
open Namecheck
open Typecheck
open IR
open IRgen
open AsmIr
(*comment just to grade, XD*)

let usage_msg = "cigrid [--pretty-print] <filename>"
let pretty_print = ref false 
let pretty_tok = ref false
let name_analysis = ref false
let precise_error = ref false
let type_check = ref false
let ir_print = ref false
let input_file = ref ""
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
      ("--ir", Arg.Set ir_print, "pretty-print the ir representation")]
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

   let ir = convert_AST ast in

   if !ir_print then ( printf "%s"  (pprint_ir_global ir));

   let asm = (InstrSelection.ir_global_to_asm ir) in 
   
   printf "\tglobal main \n\tsection .text\n%s" (pprint_func asm);

   exit 0
   
   
   