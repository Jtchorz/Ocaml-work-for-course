(*the goal of this file is to be able to take in ast and return an IR tree? for now just do one block with main*)
open IR
open Ast
open Printf

let buf = Buffer.create 16 
let cnt = ref 0

let rec create_block_list lastName name acc prevlist = function
  | SScope(stlist,ln)::[] -> 
    create_block_list lastName name acc prevlist stlist
  | SScope(stlist,ln)::_ ->
    failwith "this is SSCope assert in IRgen"

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
      create_block_list None s3 nacc3 [] restlist 

    | None -> 
      let (s1, s2) = (sprintf "_if_true%d" !cnt, sprintf "_after_if%d" ((!cnt)+1) ) in
      let nacc = (IBlock(name, (List.rev prevlist, ISBranch(eCond, s1, s2 , ln)), ln)::acc) in 
      incr cnt;incr cnt;
      let nacc2 = create_block_list (Some(s2)) s1 nacc [] [st] in 
      create_block_list None s2 nacc2 [] restlist
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
(*this section handles one line statements which are not a scope.
longer straight line code will be handled inside of a scope
//////////////////////////////////////////////////////////////////////*)
  | SExpr(e,ln)::restlist -> 
    create_block_list lastName name acc (ISExpr(e,ln)::prevlist) restlist
  | SVarDef(t, s, e,ln)::restlist -> 
    create_block_list lastName name acc (ISVarAssign(s,e,ln)::ISVarDecl(s,t,ln)::prevlist) restlist
  | SVarAssign(s,e,ln)::restlist -> 
   create_block_list lastName name acc (ISVarAssign(s,e,ln)::prevlist) restlist
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
  | _ -> failwith "some things are not implemented yet"

let convert_global = function
  | GFuncDef(t,s,listTySt,st,ln) -> let blist = create_block_list None s [] [] [st] in
    Some(IFunc(s,(t, listTySt, List.rev blist), ln))
  | GFuncDecl(t,s,listTySt,_) -> 
    Buffer.add_string buf ("\textern\t"^s^"\n");
    None
  | GVarDef(t,s,e,_) -> failwith "GVarDefTODO"
  | GVarDecl(t, s,_) -> failwith "GVarDeclTODO"
  | Gstruct(s, listTySt,_) -> failwith "GStructTODO"

(*for now assume that only one function is there*)
let convert_AST = function
  | Prog(globalList) -> (
    let rec work acc = function
    | glAst::[] -> (
      let nacc = match convert_global glAst with 
      | Some(func) ->func::acc
      | None -> acc
      in
      (List.rev nacc, Buffer.contents buf)
    )
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
