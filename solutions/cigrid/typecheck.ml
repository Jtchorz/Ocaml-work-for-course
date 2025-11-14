open Printf
open Ast 
open Hashtbl
exception DoubleDecl of string * int
exception UndeclaredVariable  of string * int
exception UndeclaredFunction of string * int
exception UndeclaredStruct of string * int
exception TypeMismatch of string * int
exception IndexTypeMismatch of string*int
exception ConditionMismatch of int
exception IllegalPointerOperation of int
let decl : (string, ty list) Hashtbl.t = create 20
let def : (string, ty list) Hashtbl.t = create 20
let structs : (string, (ty*string) list) Hashtbl.t = create 20


let typeOf hash s ln =
    match find_opt hash s with 
    | Some(t) ->  t
    | None -> (*also here try to find it in other places*)
      (match find_opt decl s with
      | Some(t::_) ->  t
      | Some([]) -> printf "variable without type"; exit 999
      | None -> 
        (match find_opt def s with
        | Some(t::_) ->  t
        | Some([]) -> printf "variable without type"; exit 999
        | None -> 
          raise (UndeclaredVariable(s, ln)) 
          )
      )

let structType structName sopt ln=
  match sopt with
  | Some(s2) -> 
    (match find_opt structs structName with
    | Some(pairList) -> 
      (match List.find_opt (fun (_,name) -> name = s2) pairList with
      | Some(ty, name) -> ty
      | None -> raise (TypeMismatch(structName, ln))
      )
    | None -> 
      raise (TypeMismatch(structName, ln))
    )
  | None -> raise (TypeMismatch(structName, ln))


let cmp t1 t2 =
  match t1 with
  | TChar | TInt -> (t2 = TChar)||(t2 = TInt)
  | TVoid -> t2 = TVoid
  | TIdent(s1) ->( match t2 with
    | TIdent(s2) -> s1 = s2
    | _ -> false
  )
  | TPoint(_) -> t1 = t2


let rec type_check_expr hash e =
  match e with
  | EVar(s, ln) -> typeOf hash s ln
  | EInt(n, _) -> TInt
  | EChar(c, _) -> TChar
  | EString(s, _) -> TPoint(TChar)
  | EBinOp(bop, e1, e2, ln) -> 
    let ty = type_check_expr hash e1 in
    let ty2 = type_check_expr hash e2 in 
    if cmp ty ty2 then 
      (match ty with 
      | TPoint(_) when (bop = BopEqual)||(bop = BopNotEq) -> ty
      | TPoint(_) -> raise (IllegalPointerOperation(ln))
      | _ -> ty
      )
    else
      raise (TypeMismatch("One of binary operators", ln))

  | EUnOp(uop, e, ln) -> 
    let ty = type_check_expr hash e in
    (match ty with 
      | TPoint(_) ->  raise (IllegalPointerOperation(ln))
      | _ -> ty 
      )

  | ECall(s, exprList, ln) ->  
    (match find_opt decl s with 
    | Some(retTy::l1) -> (
      let types  = List.map (fun expr -> type_check_expr hash expr) exprList in 
      if List.for_all2 cmp l1 types then
        retTy 
      else
        raise (TypeMismatch(s, ln))
      )
    | None | Some([]) -> (match find_opt def s with 
      | Some(retTy::l1) -> ( 
        let types  = List.map (fun expr -> type_check_expr hash expr) exprList in 
        if List.for_all2 cmp l1 types then
          retTy 
        else
          raise (TypeMismatch(s, ln))
      )
      | None | Some([]) -> raise (UndeclaredFunction(s,ln))
      )
    )

  | ENew(ty, e, ln) -> 
    let ty2 = type_check_expr hash e in 
    if ty2 = TInt then(
      match ty with
      | TIdent(s) -> (
        match find_opt structs s with
        | Some(_) -> TPoint(ty)
        | None -> raise(UndeclaredStruct(s, ln))
        )
      | _ -> TPoint(ty))
    else raise (TypeMismatch("New", ln))
    
(*write it being able to access global vars*)
  | EArrayAccess(s, e, sopt, ln) -> 
    let ty2 = type_check_expr hash e in 
    if ty2 = TInt then(
    (match typeOf hash s ln with 
    | TPoint(TIdent(sName)) -> 
        let ty = type_check_expr hash e in 
        let ty2 = structType sName sopt ln in 
        if cmp ty ty2 then ty 
        else raise (TypeMismatch(s, ln))
    | TPoint(ty) -> 
        let ty2 = type_check_expr hash e in 
        if cmp ty ty2 then ty 
        else raise (TypeMismatch(s, ln))
    | _ -> raise (TypeMismatch(s, ln))
    ))


let rec type_check_stmt hash st = 
  match st with 
  | SExpr(e, _) -> (*this is juts a function call, doesnt need to check what is happening inside*)
    ignore(type_check_expr hash e); 
    hash 

  | SVarDef(t, s, e, ln) -> 
    let exprTy = type_check_expr hash e in 
    (match find_opt hash s with (*don't check in global variables as shadowing is allowed*)
    | Some(_) -> 
      raise (DoubleDecl(s,ln))
    | None ->  
      if cmp t exprTy then(
      add hash s t; hash 
      ) else 
        raise (TypeMismatch(s, ln))
    )

  | SVarAssign(s,e,ln) -> 
    let exprTy = type_check_expr hash e in 
    let varTy = typeOf hash s ln in
    if cmp varTy exprTy then 
      hash
    else 
      raise (TypeMismatch(s, ln))

  | SArrayAssign(s,e1,sopt,e2, ln) -> 
    let exprTy = type_check_expr hash e2 in
    let indexTy = type_check_expr hash e1 in 
    if indexTy != TInt then
      raise (IndexTypeMismatch(s, ln))
    else (
      let arrTy = typeOf hash s ln in 
      match arrTy with 
      | TPoint(ty) -> 
        (match ty with
        | TIdent(s2) -> 
          let structField = structType s2 sopt ln in 
          if cmp structField exprTy then 
            hash 
          else 
            raise (TypeMismatch(s, ln))
        | _ -> printf "wierd stuff core %n \n" ln; exit 999
        )
      | TInt | TChar | TIdent(_) ->
          if cmp arrTy exprTy then 
          hash 
        else
          raise (TypeMismatch(s, ln))
      | TVoid -> 
          raise (TypeMismatch(s, ln))
    ) 

  | SScope(l, _) -> 
    let newhash = copy hash in 
    let rec work listSt nh  =
    match listSt with
      | st::restList -> work restList (type_check_stmt nh st)
      | [] -> ()
    in work l newhash;
    hash (*return the original has when a scope is ended*)

  | SIf(e,st,opst, ln) -> 
    let exprTy = type_check_expr hash e in 
    if( exprTy = TVoid ) then
      raise (ConditionMismatch(ln)) 
    else
    (match opst with 
    | Some(st2) -> type_check_stmt (type_check_stmt hash st2)  st (*the else should return the same hash unmodified*)
    | None -> type_check_stmt hash st 
    )

  | SWhile(e, st, ln) -> 
    let exprTy = type_check_expr hash e in 
    if( exprTy = TVoid ) then
      raise (ConditionMismatch(ln))
    else
      type_check_stmt hash st

  | SBreak(_) -> hash
  | SReturn(eop, _) -> 
    (match eop with
    | Some(e) -> ignore(type_check_expr hash e); hash
    | None -> hash 
    )
  | SDelete(s, ln) -> 
    match find_opt hash s with 
    | Some(_) -> hash
    | None -> raise (UndeclaredVariable(s,ln))


let type_prep_stmt listTySt st =
    let hash = create (List.length listTySt) in
    List.iter (fun(x,y) -> add hash y x ) listTySt;
    ignore(type_check_stmt hash st); ()

let type_check_global = function
  | GFuncDef(t,s,listTySt,st, ln) -> 
      let sgnt = t::(List.map fst listTySt) in
      (match find_opt decl s with 
      | Some(sgntfound) ->( 
        if sgnt = sgntfound then ()
        else
          raise (TypeMismatch(s,ln))
        )
      | None -> ()
      ); (*check if signatures match*)
        (*now check if not declared before*)
      (match find_opt def s with 
      | Some(_) -> 
          raise (DoubleDecl(s,ln))
      | None -> 
        add def s (t::(List.map fst listTySt));  
        type_prep_stmt listTySt st; 
        ()
      )

  | GFuncDecl(t,s,listTySt, ln) -> 
    (match find_opt decl s with
    | Some(_) -> 
        raise (DoubleDecl(s,ln))
    | None -> add decl s (t::(List.map fst listTySt)); 
      ()
    )

  | GVarDef(t,s,e, ln) -> 
      (match find_opt decl s with 
      | Some(varfound) ->( 
        if [t] = varfound then ()
        else
          raise (TypeMismatch(s,ln))
          )
      | None -> ()
      ); (*check if signatures match*)
        (*now check if not declared before*)
      (match find_opt def s with
      | Some(_) -> 
        raise (DoubleDecl(s,ln))
      | None -> 
        add def s [t]; 
        ()
      )

  | GVarDecl(t, s, ln) -> 
    (match find_opt decl s with
    | Some(_) -> 
      raise (DoubleDecl(s,ln))
    | None -> 
      add decl s [t];
       ()
    )

  | Gstruct(s, listTySt, ln) -> 
    (match find_opt structs s with
    | Some(_) -> 
      raise (DoubleDecl(s,ln))
    | None -> 
      add structs s listTySt;
      ()
    )

let type_check_program = function
  | Prog(globalList) -> List.iter (type_check_global) globalList
  