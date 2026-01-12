open Hashtbl
open AsmIr
open Printf


let iNum = ref 0 (*the number of instruction, it's just easier to do it as global*)
let changed = ref true (*the flag that is tripped if anything in the live_in live_out changes*)

(*record for the node, def and use could be single ints because we are doing it instruction by instruction*)
type node = {
  name : string;
  succesors : string list;
  def : int list;
  use : int list;
  mutable live_in : int list;
  mutable live_out : int list;
}

let graph : (string, node) Hashtbl.t = create 100

(*this function defines everything besides live_in and live_out
only virtual registers need to be considered as others won't create conflicts
this only handles straight line code*)
let rec instr_to_node instr = 
  let def = 
  (match instr with 
    | BinOp(bop, op1, op2) -> (
        match op1 with 
        | TReg((n,_),_) -> [n]
        | Imm(_) | Reg(_) | Mem(_) | GString(_) | GVar(_) | NoOp -> [])
    | UnOp(_) | Call(_) | Cqo -> [] (*matching UnOP as empty def is a case in which I follow the example in the instructions*)
  ) in 
  let use = 
  (match instr with 
    | BinOp(_, _, op) | UnOp(_, op) -> 
      (match op with 
      | TReg((n,_),_) -> [n]
      | Imm(_) | Reg(_) | Mem(_) | GString(_) | GVar(_) | NoOp -> [])
    | Call(_) | Cqo -> [] 
  ) in
  let name = "inst"^(Printf.sprintf "%02d" !iNum) in (*this node is the instruction*)
  incr iNum;
  let succesors = ["inst"^(Printf.sprintf "%02d" !iNum)] in (*and the successor is always the next one, jmps can only be in blockends*)
  Hashtbl.add graph name {name; succesors; def; use; live_in = []; live_out =[];}

(*blockends can jump but they don't use any virtual registers*)
let blockend_to_node blEnd =
  let succesors = (match blEnd with 
  | Ret -> []
  | Jmp(s) -> [s]
  | JBinOp(_,s1,s2) -> [s1;s2])
  in 
  let name = "inst"^(Printf.sprintf "%02d" !iNum) in
  incr iNum;
  Hashtbl.add graph name {name; succesors; def=[]; use=[]; live_in = []; live_out =[];}

(*this will add the block first, so it's easy to jump to here from blockends
then it will analyze the straight line code and the blockend
should handle only one block*)
 let unpack_block = function 
  | Block(s,(instList, blEnd)) -> 
    Hashtbl.add graph s {name="_"^s; succesors=["inst"^(Printf.sprintf "%02d" !iNum)]; def=[];use=[];live_in=[];live_out=[];};
    List.iter instr_to_node instList;
    blockend_to_node blEnd
     (*blENd is literally just a jmp at this point, doesnt need to be spilled*)

(*this just runs the function for all the blocks inside, and thus links them up*)
let unpack_func = function 
    | Func(s, bList)-> 
    (*I don't add the node for the function here, as it would be duplicating with the first block*)
    List.iter unpack_block bList

(*this takes all the functions, and unpacks them all*)
let generate_graph func_list = 
  List.iter unpack_func func_list


  (*this is just a prettyprinter for printing all the variables
  %02d is so that after alphabetically sorting it's easy to read (adds leading zero)*)
let print_vars varList = 
  String.concat ", " (List.map (fun n -> (Printf.sprintf "vreg%02d" n)) varList)

(*prettyprinter with an acc for folding*)
let print_node key ({ name; succesors; def; use; live_in; live_out })  acc =
    (sprintf "Node(%s, succ = {%s}, def = {%s}, use = {%s}, live_in = {%s}, live_out = {%s}) \n" name (String.concat ", " succesors)
    (print_vars def)
    (print_vars use)
    (print_vars live_in)
    (print_vars live_out))::acc

let print_graph () = 
  let arr = Hashtbl.fold print_node graph [] in (*folding in order to produce a list of strings because I cant "List.map" a hashtable*)
  let stri = String.concat "\n" (List.sort String.compare arr) in  (*sort it to produce readable output*)
  printf "%s\n" stri

(*returns the live_in for all the successors*)
let succ_in name =
    let n = (try Hashtbl.find graph name with 
    | _ -> printf "%s" name; exit 9)
    in 
    n.live_in 

(*this is the actual algorithm as described on slides, it creates the live_in and live_out, unduplicates them
and also flips the changed flag if needed, doesn't write the same data if not needed*)
let live_upd key ({ name; succesors; def; use; live_in; live_out }) =
  let out_dup = List.concat_map succ_in succesors in(*add here if duplicates become a problem*)
  let out_new = List.sort_uniq Int.compare out_dup in  (*this sorts it for <> operator and removes duplicates*)

  let in_dup = use@(List.filter (fun x -> not (List.mem x def)) out_new) in  
  let in_new = List.sort_uniq Int.compare in_dup in

  if (out_new <> live_out) || (in_new <> live_in) then (
    changed := true ;
    let n = Hashtbl.find graph key in
    n.live_in <- in_new ;
    n.live_out <- out_new
  )

(*as long as there was at least one change anywhere, continue the algorithm*)
let liveness () = 
  while (!changed) do
    changed := false;
    Hashtbl.iter live_upd graph
  done

(*overall function that makes following the structure easier*)
let analyze asm =  
  generate_graph asm;  
  print_graph ();
  liveness ();
  print_graph ()

