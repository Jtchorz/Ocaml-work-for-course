(*the goal of this file is to be able to take in ast and return an IR tree? for now just do one block with main*)
open IR
open Ast
open Printf

let buf = Buffer.create 16 
let cnt = ref 0

(*this function handles insides of a functions.
lastName is optional, as that decides if we are at the end of a block with a predefined
destination later or not
name is mandatory and holds the name of the block we creating
acc holds previous blocks
prevlist holds previous straight line code 
the func matches straight code until it reaches a block end, then
it creates a previous block and all its necessary blocks and then throws the rest of the
code to create a new block with a specified name
when list is empty it either creates a return or jump blockend depending on lastname*)

let rec create_block_list lastName name acc prevlist = function
  | SScope(stlist,ln)::[] -> 
    create_block_list lastName name acc prevlist stlist  
  | SScope(stlist,ln)::restlist ->
    let s = sprintf "_scope%d" !cnt in 
    incr cnt;
    let nacc = create_block_list (Some(s)) name acc prevlist stlist in
    create_block_list lastName s nacc [] restlist 

  | SReturn(eop, ln)::restlist -> 
    IBlock(name, (List.rev prevlist, ISReturn(eop,ln)), ln)::acc

  | SIf(eCond, st,stopt,ln)::restlist ->(
    match stopt with 
    | Some(st2) -> 
      let (s1, s2, s3) = (sprintf "_if_true%d" !cnt, 
      sprintf "_if_false%d" ((!cnt)+1), 
      sprintf "_after_if%d" ((!cnt)+2) ) in
      let nacc = (IBlock(name, (List.rev prevlist, ISBranch(eCond, s1, s2 , ln)), ln)::acc) in 
      incr cnt;incr cnt;incr cnt;
      let nacc2 = create_block_list (Some(s3)) s1 nacc [] [st] in
      let nacc3 = create_block_list (Some(s3)) s2 nacc2 [] [st2] in 
      create_block_list lastName s3 nacc3 [] restlist 

    | None -> 
      let (s1, s2) = (sprintf "_if_true%d" !cnt, sprintf "_after_if%d" ((!cnt)+1) ) in
      let nacc = (IBlock(name, (List.rev prevlist, ISBranch(eCond, s1, s2 , ln)), ln)::acc) in 
      incr cnt;incr cnt;
      let nacc2 = create_block_list (Some(s2)) s1 nacc [] [st] in 
      create_block_list lastName s2 nacc2 [] restlist
  )
  
  | SWhile(eCond, st, ln)::restlist ->  (
    let (s1, s2, s3) = (sprintf "_while_body%d" !cnt, 
    sprintf "_after_while%d" ((!cnt)+1), 
    sprintf "_while_cond%d" ((!cnt)+2) ) in
    let nacc = (IBlock(name, (List.rev prevlist, ISBranch(eCond, s1, s2 , ln)), ln)::acc) in 
    incr cnt;incr cnt;incr cnt; 
    let nacc2 = create_block_list (Some(s3)) s1 nacc [] [st] in
    let nacc3 = IBlock(s3,([],ISBranch(eCond,s1,s2,ln)),ln)::nacc2 in
    create_block_list lastName s2 nacc3 [] restlist 
  )
(*this handles straight line code, by storing it in an accumulator
//////////////////////////////////////////////////////////////////////*)
  | SExpr(e,ln)::restlist -> 
    create_block_list lastName name acc (ISExpr(e,ln)::prevlist) restlist
  | SVarDef(t, s, e,ln)::restlist -> 
    create_block_list lastName name acc (ISVarAssign(s,e,ln)::ISVarDecl(s,t,ln)::prevlist) restlist
  | SVarAssign(s,e,ln)::restlist -> 
   create_block_list lastName name acc (ISVarAssign(s,e,ln)::prevlist) restlist
  | SArrayAssign(s,e,sopt,eVal,ln)::restlist ->(
    match sopt with
    | None -> create_block_list lastName name acc (ISArrayAssign(s,e,eVal,ln)::prevlist) restlist
    | Some(_) -> failwith "not implementing structs IRgen here?"
  )
  | SDelete(s,ln)::restlist ->
    create_block_list lastName name acc (ISDelete(s,ln)::prevlist) restlist

  | SBreak(ln)::restlist -> failwith "not implementing break IRgen"
(*//////////////////////////////////////////////////////////////////////////////*)
  | [] -> (
    match lastName with 
    | Some(s) ->
      let nacc = IBlock(name,(List.rev prevlist, ISJump(s,-1)),-1)::acc in
      nacc 
    | None ->
      let nacc = IBlock(name,(List.rev prevlist, ISReturn(None,-1)),-1)::acc in
      nacc
  )

(*this will handle different things, not structs. 
Function defs will go up to get blocked and declaratins will be added to extern 
variable definitions will be passed to instrselection, as they handle the .data section*)
let convert_global = function
  | GFuncDef(t,s,listTySt,st,ln) -> 
    (*the floors here are to enable argument passing*)
    let blist = create_block_list None ("_"^s^"_") [] [] [st] in
    Some(IFunc(s,(t, listTySt, List.rev blist), ln))

  | GFuncDecl(t,s,listTySt,_) -> 
    Buffer.add_string buf ("\textern\t"^s^"\n");
    None

  | GVarDef(t,s,e,ln) -> (
    match t with 
    | TInt | TChar | TPoint(_) -> Some(IGVarDef(t,s,e,ln))
    | TIdent(_) -> failwith "not implementing structs IRgen" 
    | TVoid -> failwith (sprintf "tried declaring variable of type void at line %d" ln)
  )

  | GVarDecl(t, s,_) -> 
    Buffer.add_string buf ("\textern\t"^s^"\n");
    None

  | Gstruct(s, listTySt,_) -> failwith "not implementing structs IRgen"

(*this gets all of the globals and matches them to correct blocks 
If it is just a declaration it will not pass it to IR, but return a string of extersn
*)
let convert_AST = function
  | Prog(globalList) -> (
    let rec work acc = function
    | glAst::[] -> (*this looks redundant, but is importnat to be able to handle an empty file*)
      let nacc = match convert_global glAst with 
      | Some(func) ->func::acc
      | None -> acc
      in
      (List.rev nacc, Buffer.contents buf)
    | glAst::restlist -> (
      let nacc = match convert_global glAst with 
      | Some(func) ->func::acc
      | None -> acc
      in
      work nacc restlist
      )
    | [] -> ([IFunc("",(TVoid, [], [IBlock("",([],ISReturn(None,0)),0)]), 0)],"")
    in work [] globalList
  )