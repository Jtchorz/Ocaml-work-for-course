open Printf
open Ast 
open Hashtbl(*
exception DoubleDecl  of string
exception UndeclaredVariable  of string

(*Our hash table contains a list of ty, the first one is the return type
the rest are variables*)
(*
let type_check_global g hash =
  match g with
  | GFuncDef(t,s,listTySt,st) -> hash
  | GFuncDecl(t,s,listTySt) -> 
    match find_opt hash s with
    | Some(_) -> raise DoubleDecl(s)
    | None -> add hash s t::List.map fst listTySt
  | GVarDef(t,s,e) -> "GVarDef(" ^ (pprint_ty t) ^ ", " ^ (string_print s) ^ ", " ^ (pprint_expr e) ^ ")" ^ "\n"
  | GVarDecl(t, s) -> "GVarDecl(" ^ (pprint_ty t) ^ ", " ^ (string_print s) ^ ")" ^ "\n"
  | Gstruct(s, listTySt) -> "GStruct(" ^ (string_print s) ^ ", " ^ (pairList_print listTySt) ^ ")" ^ "\n"

let type_check_program prog =
  let hash = create (List.length prog) in
  let rec work pr h= 
    match pr with
    | g::rest_pr -> let newh = check_global g hash in work rest_pr newh
    | [] -> 0
  in work prog hash
*)
let rec name_check_expr hash e =
  match e with
  | EVar(s) ->
    (match find_opt hash s with
    | Some(_) -> ()
    | None -> printf "undeclared variable %s \n" s; exit 2
    )
  | EInt(n) -> ()
  | EChar(c) -> ()
  | EString(s) -> ()
  | EBinOp(bop, e1, e2) -> name_check_expr hash e1; name_check_expr hash  e2
  | EUnOp(uop, e) -> name_check_expr hash e
  | ECall(s, l) -> 
    let rec work l =
      match l with
      | e::restl -> name_check_expr hash e; work restl 
      | [] -> ()
    in work l
  | ENew(t, e) -> name_check_expr hash e
  | EArrayAccess(s, e, sopt) -> 
    (match find_opt hash s with 
    | Some(_) -> name_check_expr hash e; 
    | None ->  printf "undeclared array name%s \n" s; exit 2
    )
 let rec type_check_stmt hash st =
  match st with 
  | SExpr(e) -> 
    Pair.make (hash, type_check_expr hash e)
    
  | SVarDef(t, s, e) ->
    let exprTy = type_check_expr hash e in
    (match find_opt hash s with
    | Some(_) -> printf "Double declaration of a variable %s \n" s; exit 2
    | None ->  
      if t = exprTy then(
        add hash s t; 
        hash
      )
      else
        printf "type mismatch"
    )

  | SVarAssign(s,e) -> 
    type_check_expr hash e; 
    (match find_opt hash s with
    | Some(_) -> hash
    | None -> printf "undeclared variable %s \n" s; exit 2
    )
  | SArrayAssign(s,e1,sopt,e2) -> 
    name_check_expr hash e1;
    name_check_expr hash e2;
    (match find_opt hash s with
    | Some(_) -> hash
    | None -> printf "undeclared variable %s \n" s; exit 2
    )
  | SScope(l) -> 
    let newhash = copy hash in 
    let rec work listSt nh  =
    match listSt with
      | st::restList -> work restList (name_check_stmt nh st)
      | [] -> ()
    in work l newhash;
    hash (*return the original has when a scope is ended*)
  | SIf(e,st,opst) -> 
    name_check_expr hash e;
    (match opst with
    | Some(st2) -> name_check_stmt (name_check_stmt hash st2)  st (*the else should return the same hash unmodified*)
    | None -> name_check_stmt hash st
    )
  | SWhile(e, st) -> 
    name_check_expr hash e;
    name_check_stmt hash st

  | SBreak -> hash
  | SReturn(eop) -> 
    (match eop with
    | Some(e) -> name_check_expr hash e; hash
    | None -> hash
    )
  | SDelete(s) -> 
    match find_opt hash s with
    | Some(_) -> hash
    | None -> printf "undeclared variable %s \n" s; exit 2

let name_prep_stmt listTySt st =
    let hash = create (List.length listTySt) in
    List.iter (fun(x,y) -> add hash y x ) listTySt;
    ignore(name_check_stmt hash st); ()

let name_check_global  = function
  | GFuncDef(t,s,listTySt,st) -> name_prep_stmt listTySt st
  | GFuncDecl(t,s,listTySt) -> ()
  | GVarDef(t,s,e) -> ()
  | GVarDecl(t, s) -> ()
  | Gstruct(s, listTySt) -> ()
*)
let name_check_program ast = ()(*function
  | Prog(globalList) -> List.iter (name_check_global) globalList*)
  