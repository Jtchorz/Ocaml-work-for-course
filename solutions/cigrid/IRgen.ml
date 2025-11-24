(*the goal of this file is to be able to take in ast and return an IR tree? for now just do one block with main*)
open IR
open Ast
open Printf

let rec st_to_irSt acc = function
  | SExpr(e,ln) ->  ((ISExpr(e,ln)::acc),ln)
  | SVarDef(t, s, e,ln) -> (([ISVarAssign(s,e,ln); ISVarDecl(s,t,ln)]@acc),ln)
  | SVarAssign(s,e,ln) -> ((ISVarAssign(s,e,ln)::acc),ln)
  | SArrayAssign(s,e1,sopt,e2,_) -> failwith "SArrayAssignTODO" 
  | SScope(l,ln) -> failwith "no scope in scope yet" 
  | SIf(e,st,opst,_) -> failwith "SIfTODO" 
  | SWhile(e, st,_) ->  failwith "SWhileTOFO" 
  | SBreak(_) -> failwith "SBreakTODO" 
  | SReturn(eop,ln) -> failwith "really shouldn't be here, logic error"
  | SDelete(s,_) -> failwith "SDeleteTODO" 

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
    | _ -> failwith "FuncDef doesn't contain a scope\n"
let convert_global = function
  | GFuncDef(t,s,listTySt,st,ln) -> IFunc("main",(t, listTySt, create_block_list st), ln)
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
