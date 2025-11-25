(*the goal of this file is to be able to take in ast and return an IR tree? for now just do one block with main*)
open IR
open Ast
open Printf

let cnt = ref 0

(*make this handle oneliners too????*)
let straight_code stlist =
  let rec work acc = function
  | st::restlist -> (
    match st with 
    | SExpr(e,ln) -> 
      work (ISExpr(e,ln)::acc) restlist
    | SVarDef(t, s, e,ln) -> 
      work (ISVarAssign(s,e,ln)::ISVarDecl(s,t,ln)::acc) restlist 
    | SVarAssign(s,e,ln) -> 
      work (ISVarAssign(s,e,ln)::acc) restlist
    | _ -> (acc, st::restlist)
    ) 
  | [] -> (acc, [])
  in let (acc, restlist) = work [] stlist 
  in (List.rev acc, restlist)

  (*what is this function doing even????
  it will take a list of statements and produce a block for it
  it needs the name of the block, where the block should jump if done
  an accumulator to add to the block and if needed the list of the
  irstatements before it
  it has to be able to match straight code 

STRAIGHT CODE IN THIS FUNCTION CAN ONLY BE ONELINERS
  if there is none lastName then we just throw in a ISreturn of nothing 
  else jump to lastName 
  if just a return then create a block with given prevlist(can be empty on purpose)
    and given name and appropriate return
  if a scope then 
  *)

let rec create_block_list lastName name acc prevlist = function
  | SScope(stlist,ln)::restlist -> 
    let (newIrList, inScopeRest) = straight_code stlist in
    create_block_list lastName name acc newIrList inScopeRest

  | SReturn(eop, ln)::restlist -> 
    IBlock(name, (prevlist, ISReturn(eop,ln)), ln)::acc

  | SIf(eCond, st,stopt,ln)::restlist ->(
    match stopt with 
    | Some(st2) -> 
      let (s1, s2, s3) = (sprintf "_if_true%d" !cnt, 
      sprintf "_if_false%d" ((!cnt)+1), 
      sprintf "_after_if%d" ((!cnt)+2) ) in
      let nacc = (IBlock(name, (prevlist, ISBranch(eCond, s1, s2 , ln)), ln)::acc) in 
      incr cnt;incr cnt;incr cnt;
      let nacc2 = create_block_list (Some(s3)) s1 nacc [] [st] in
      let nacc3 = create_block_list (Some(s3)) s2 nacc2 [] [st2] in 
      create_block_list None s3 nacc3 [] restlist 

    | None -> 
      let (s1, s2) = (sprintf "_if_true%d" !cnt, sprintf "_after_if%d" ((!cnt)+1) ) in
      let nacc = (IBlock(name, (prevlist, ISBranch(eCond, s1, s2 , ln)), ln)::acc) in 
      incr cnt;incr cnt;
      let nacc2 = create_block_list (Some(s2)) s1 nacc [] [st] in 
      create_block_list None s2 nacc2 [] restlist
  )
  | SWhile(eCond, st, ln)::restlist ->  (
    let (s1, s2, s3) = (sprintf "_while_body%d" !cnt, 
    sprintf "_after_while%d" ((!cnt)+1), 
    sprintf "_while_cond%d" ((!cnt)+2) ) in
    let nacc = (IBlock(name, (prevlist, ISBranch(eCond, s1, s2 , ln)), ln)::acc) in 
    incr cnt;incr cnt;incr cnt; 
    let nacc2 = create_block_list (Some(s3)) s1 nacc [] [st] in
    let nacc3 = IBlock(s3,([],ISBranch(eCond,s1,s2,ln)),ln)::nacc2 in
    create_block_list None s2 nacc3 [] restlist 
  )
(*this section handles one line statements which are not a scope.
longer straight line code will be handled inside of a scope
//////////////////////////////////////////////////////////////////////*)
  | SExpr(e,ln)::[] -> (
    match lastName with 
    | Some(lname) -> IBlock(name,([ISExpr(e,ln)],ISJump(lname, ln)),ln)::acc
    | None -> IBlock(name,([ISExpr(e,ln)],ISReturn(None,ln)),ln)::acc
    ) 
  | SExpr(_)::_ -> failwith "this is an assert for SExpr in IRgen.ml"

  | SVarDef(t, s, e,ln)::[] -> (
    match lastName with 
    | Some(lname) -> IBlock(name,([ISVarAssign(s,e,ln);ISVarDecl(s,t,ln)],ISJump(lname, ln)),ln)::acc
    | None -> IBlock(name,([ISVarAssign(s,e,ln);ISVarDecl(s,t,ln)],ISReturn(None,ln)),ln)::acc
    ) 
  | SVarDef(_)::_ -> failwith "this is an assert for SVarDef in IRgen.ml"
(*the asserts here showed that after a block of while or whatever, we can have
straight line code in not just one stmt, these makes the need to handle these cases differently
maybe just create a block from the first one, and handle restnormally???*)
  | SVarAssign(s,e,ln)::[] -> (
    match lastName with 
    | Some(lname) -> IBlock(name,([ISVarAssign(s,e,ln)],ISJump(lname, ln)),ln)::acc
    | None -> IBlock(name,([ISVarAssign(s,e,ln)],ISReturn(None,ln)),ln)::acc
    ) 
  | SVarAssign(_)::stmtlist -> printf "aaa\n%saaa\n" (String.concat "; \n" (List.map pprint_stmt stmtlist));failwith "this is an assert for SVarAssign in IRgen.ml"
(*//////////////////////////////////////////////////////////////////////////////*)
  | [] -> (
    match lastName with 
    | Some(s) -> IBlock(name, (prevlist, ISJump(s,-1)), -1)::acc
    | None -> 
      printf "wierd case, you didnt think this through";
      IBlock(name, (prevlist, ISReturn(None, -1)), -1)::acc
  )
  | _ -> failwith "nothing implemented yet"

let convert_global = function
  | GFuncDef(t,s,listTySt,st,ln) -> let blist = create_block_list None s [] [] [st] in
    IFunc(s,(t, listTySt, List.rev blist), ln)
  | GFuncDecl(t,s,listTySt,_) -> failwith "GFuncDeclTODO"
  | GVarDef(t,s,e,_) -> failwith "GVarDefTODO"
  | GVarDecl(t, s,_) -> failwith "GVarDeclTODO"
  | Gstruct(s, listTySt,_) -> failwith "GStructTODO"

(*for now assume that only one function is there*)
let convert_AST = function
  | Prog(globalList) -> (
    match globalList with
    | [glAst] -> convert_global glAst
    | _ -> failwith "Not allowed to do more than one function."
  )
