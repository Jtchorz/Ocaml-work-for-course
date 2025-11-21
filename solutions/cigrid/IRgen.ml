(*the goal of this file is to be able to take in ast and return an IR tree? for now just do one block with main*)
open IR
open Ast
open Printf

let rec st_to_irSt acc = function
  | SExpr(e,ln) ->  ((ISExpr(e,ln)::acc),ln)
  | SVarDef(t, s, e,ln) -> (([ISVarAssign(s,e,ln); ISVarDecl(s,t,ln)]@acc),ln)
  | SVarAssign(s,e,ln) -> ((ISVarAssign(s,e,ln)::acc),ln)
  | SArrayAssign(s,e1,sopt,e2,_) -> printf "SArrayAssignTODO" ; exit 3
  | SScope(l,ln) -> printf "no scope in scope yet" ; exit 3
  | SIf(e,st,opst,_) -> printf "SIfTODO" ; exit 3
  | SWhile(e, st,_) ->  printf "SWhileTOFO" ; exit 3
  | SBreak(_) -> printf "SBreakTODO" ; exit 3
  | SReturn(eop,ln) -> printf "really shouldn't be here, logic error" ; exit 999
  | SDelete(s,_) -> printf "SDeleteTODO" ; exit 3

let create_block_list = function
    | SScope(stlist,lnTop) -> (
      let rec work acc =function
        | st::restlist -> (
          match st with
          | SReturn(eop, ln) -> (acc,ISReturn(eop,ln))
          | _ -> let (nacc,ln) = st_to_irSt acc st in work nacc restlist
          )
        | [] -> (acc,ISReturn(Some(EInt(0, lnTop)),lnTop))
    in let (acc,blockend) = work [] stlist 
    in [IBlock("main",((List.rev acc),blockend),lnTop)]
    ) 
    | _ -> printf "FuncDef doesn't contain a scope\n"; exit 3
let convert_global = function
  | GFuncDef(t,s,listTySt,st,ln) -> IFunc("main",(t, listTySt, create_block_list st), ln)
  | GFuncDecl(t,s,listTySt,_) -> printf "GFuncDeclTODO"; exit 3
  | GVarDef(t,s,e,_) -> printf "GVarDefTODO"; exit 3
  | GVarDecl(t, s,_) -> printf "GVarDeclTODO"; exit 3
  | Gstruct(s, listTySt,_) -> printf "GStructTODO"; exit 3

(*for now assume that only one function is there*)
let convert_AST = function
  | Prog(globalList) -> (
    match globalList with
    | [glAst] -> convert_global glAst
    | _ -> printf "Not allowed to do more than one function."; exit 3
  )
