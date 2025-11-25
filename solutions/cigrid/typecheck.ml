open Printf
open Ast 
open Hashtbl
exception DoubleDecl of string * int
exception UndeclaredVariable  of string * int
exception UndeclaredFunction of string * int
exception UndeclaredStruct of string * int
exception TypeMismatch of string * int
exception NonexistentType of string*int
exception IndexTypeMismatch of string*int
exception ConditionMismatch of int
exception IllegalPointerOperation of int
let decl : (string, ty list) Hashtbl.t = create 20
let def : (string, ty list) Hashtbl.t = create 20
let structs : (string, (ty*string) list) Hashtbl.t = create 20


let typeOf hash varName ln =
    match find_opt hash varName with 
    | Some(t) ->  t
    | None -> (*also here try to find it in other places*)
      (match find_opt decl varName with
      | Some(t::_) ->  t
      | Some([]) -> printf "variable without type"; exit 999
      | None -> (
        match find_opt def varName with
        | Some(t::_) ->  t
        | Some([]) -> printf "variable without type"; exit 999
        | None -> 
          raise (UndeclaredVariable(varName, ln)) 
          )
      )

let fieldType hash structName fieldName ln =
  let typeOfStruct = (
    match typeOf hash structName ln with
    | TPoint(TIdent(s)) -> s 
    | _ ->  raise (TypeMismatch(structName, ln))
    )
  in
  match fieldName with
  | Some(s2) -> 
    (match find_opt structs typeOfStruct with
    | Some(pairList) -> 
      (match List.find_opt (fun (_,name) -> name = s2) pairList with
      | Some(ty, name) -> ty
      | None -> raise (TypeMismatch(structName, ln))
      )
    | None -> 
      raise (TypeMismatch(structName, ln))
    )
  | None -> raise (TypeMismatch(structName, ln))


let rec cmp t1 t2 =
  match t1, t2 with
  | (TChar, TInt) | (TInt,TChar) | (TChar, TChar) -> true
  | (TVoid, TVoid) -> true
  | (TIdent(s1),TIdent(s2)) -> s1 = s2
  | (TInt, TPoint(_)) | (TPoint(_), TInt) -> true
  | (TPoint(ty1), TPoint(ty2)) -> cmp ty1 ty2
  | (TInt, TInt) -> true
  | (_, _) -> false

(*//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////
*)
let rec type_check_expr hash e : ty =
  match e with
  | EVar(varName, ln) -> typeOf hash varName ln
  | EInt(n, _) -> TInt
  | EChar(c, _) -> TChar 
  | EString(s, _) -> TPoint(TChar)
  | EBinOp(bop, valueL, valueR, ln) ->  (*you have to check what the binop actually results in == and != are special case I think*)
    let typeL = type_check_expr hash valueL in
    let typeR = type_check_expr hash valueR in 
    if cmp typeL typeR then 
      (match typeL with 
      | TPoint(_) when (bop = BopEqual)||(bop = BopNotEq) -> TInt
      | TPoint(_) -> raise (IllegalPointerOperation(ln))
      | _ -> (
        match bop with
        | BopAdd | BopSub | BopMult | BopDiv | BopModulo | BopBitAnd 
        | BopBitOr | BopShiftLeft | BopShiftRight -> typeL
        | _ -> TInt (*these operators should return bool tho*)
      )
      )
    else
      raise (TypeMismatch(Ast.pprint_binop bop, ln))

  | EUnOp(uop, value, ln) -> 
    let ty = type_check_expr hash value in
    (match ty with 
      | TPoint(_) ->  raise (IllegalPointerOperation(ln))
      | _ -> ty 
      )

  | ECall(fName, argVals, ln) ->  
    (match find_opt decl fName with 
    | Some(retType::declArgTypes) -> (
      let argTypes  = List.map (fun expr -> type_check_expr hash expr) argVals in 
      if List.for_all2 cmp argTypes declArgTypes then
        retType
      else
        raise (TypeMismatch(fName, ln))
      )
    | None | Some([]) -> (
      match find_opt def fName with 
      | Some(retType::defArgTypes) -> ( 
        let argTypes  = List.map (fun expr -> type_check_expr hash expr) argVals in 
        if List.for_all2 cmp argTypes defArgTypes then
          retType
        else
          raise (TypeMismatch(fName, ln))
      )
      | None | Some([]) -> raise (UndeclaredFunction(fName,ln))
      )
    )

  | ENew(arrType, indexVal, ln) -> 
    let indexType = type_check_expr hash indexVal in 
    if indexType = TInt then(
      (match arrType with 
      | TIdent(structName) -> (
        match find_opt structs structName with
        | Some(_) -> (TPoint(arrType))
        | None -> raise(UndeclaredStruct(structName, ln))
        ) 
      | TInt | TChar | TPoint(_) -> (TPoint(arrType))
      | TVoid -> raise (TypeMismatch("New can't be void", ln))
      )
    )else (raise (TypeMismatch("New", ln)))

(*write it being able to access global vars*)
  | EArrayAccess(arrName, indexVal, structField, ln) -> 
    let indexType = type_check_expr hash indexVal in 
    if indexType = TInt then(
    (match structField with 
    | Some(_) -> fieldType hash arrName structField ln
    | None -> (
      match typeOf hash arrName ln with 
      | TPoint(ty) -> ty 
      | _ -> raise (TypeMismatch(arrName, ln))
    )
    ))
    else
      raise (IndexTypeMismatch(arrName, ln))

(*/////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////*)
let rec type_check_stmt origHash st = 
  (*this is a big change because I thought ocaml was copying the hashes around, I have to change it*)
  let hash = copy origHash in 
  (*this makes only the caller to be able to modify its hash, as it will have to equate them*)
  match st with 
  | SExpr(e, _) -> (*this is juts a function call, doesnt need to check what is happening inside*)
    ignore(type_check_expr hash e); 
    hash 

  | SVarDef(varType, varName, value, ln) -> 
    let valType = type_check_expr hash value in 
    (match find_opt hash varName with (*don't check in global variables as shadowing is allowed*)
    | Some(_) -> 
      raise (DoubleDecl(varName,ln))
    | None ->  
      if cmp varType valType then(
        add hash varName varType; 
        hash 
      )else 
        raise (TypeMismatch(varName, ln))
    )
(*///////////////////////////////////////////////////*)
  | SVarAssign(varName,value,ln) -> 
    let valType = type_check_expr hash value in 
    let varType = typeOf hash varName ln in
    if cmp varType valType then 
      hash 
    else 
      raise (TypeMismatch(varName, ln))
(*///////////////////////////////////////////////////*)
  | SArrayAssign(arrName,indexVal,structField,value, ln) -> 
    let valType = type_check_expr hash value in
    let indexType = type_check_expr hash indexVal in 
    if indexType != TInt then 
      raise (IndexTypeMismatch(arrName, ln))
    else (
      match structField with 
      | Some(_) -> 
        let structFieldType = fieldType hash arrName structField ln in 
        if cmp structFieldType valType then hash 
        else raise (TypeMismatch(arrName, ln))
      | None -> (
        match typeOf hash arrName ln with 
        | TPoint(arrType) -> 
          if cmp arrType valType then hash 
          else raise (TypeMismatch(arrName, ln))
        | _ -> raise (TypeMismatch(arrName, ln))
        )
    ) 
(*///////////////////////////////////////////////////*)
  | SScope(l, _) -> 
    let newhash = copy hash in 
    let rec work listSt h  =
    match listSt with 
      | st::restList -> 
        let nh = type_check_stmt h st in
        work restList (nh)
      | [] -> ()
    in work l newhash;
    hash (*return the original has when a scope is ended*)
(*///////////////////////////////////////////////////*)
  | SIf(conditionVal,ifBody,elseBodyOpt, ln) -> 
    (*just check if condition is well typed as it acn be pretty much any type inside*)
    ignore(type_check_expr hash conditionVal);
    let ifHash = copy hash in
    (match elseBodyOpt with 
    | Some(elseBody) -> 
      let elseHash = copy hash in
      ignore(type_check_stmt ifHash ifBody); (*the else should return the same hash unmodified*)
      type_check_stmt elseHash elseBody 
    | None -> 
      type_check_stmt ifHash ifBody
    )
(*///////////////////////////////////////////////////*)
  | SWhile(conditionVal, whileBody, ln) -> 
    ignore(type_check_expr hash conditionVal);
    type_check_stmt hash whileBody

(*///////////////////////////////////////////////////*)
  | SBreak(_) -> hash
  | SReturn(returnVal, _) -> 
    (match returnVal with 
    | Some(e) -> ignore(type_check_expr hash e); hash
    | None -> hash 
    )
(*///////////////////////////////////////////////////*)
  | SDelete(varName, ln) -> 
    match find_opt hash varName with 
    | Some(_) -> hash
    | None -> raise (UndeclaredVariable(varName,ln))

(* helpers for globals
/////////////////////////////////////////////////////////////
////////////////////////////////////////////////////*)

(*this creates the enviorement within a function to be passed around
and then checks the function  body for types*)
let type_prep_stmt listTySt st ln=
    let hash = create (List.length listTySt) in
    List.iter (fun(x,y) -> 
      if (mem hash y) then 
        raise (DoubleDecl(y,ln)) 
      else
        add hash y x ) listTySt;
    ignore(type_check_stmt hash st); ()

let rec type_exists = function
    | TVoid | TInt | TChar -> true
    | TPoint(t) -> type_exists t
    | TIdent(t) -> 
      match find_opt structs t with 
      | Some(_) -> true
      | None -> false

let check_args args ln fName =
    let argTypes = List.map fst args in
    if List.for_all type_exists argTypes then argTypes
    else
      raise (NonexistentType(fName,ln))

(*globals 
////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////*)

let type_check_global = function
  | GFuncDef(fType,fName,args,body, ln) ->
      (match find_opt structs fName with
      | Some(_) -> raise (DoubleDecl(fName,ln))
      | None -> ()
      );

      let argTypes = check_args args ln fName in
      let sgnt = fType::argTypes in 
      (*if defined before, find if sgnt match*)
      (match find_opt decl fName with 
      | Some(foundSgnt) ->( 
        if sgnt = foundSgnt then ()
        else 
          raise (TypeMismatch(fName,ln))
        )
      | None -> ()
      ); 
      (*now check if not declared before*)
      (match find_opt def fName with 
      | Some(_) -> 
          raise (DoubleDecl(fName,ln))
      | None -> 
        add def fName sgnt;  
        type_prep_stmt args body ln; 
        ()
      )
(*///////////////////////////////////////////////////*)
  | GFuncDecl(fType,fName,args, ln) -> 
      (match find_opt structs fName with
      | Some(_) -> raise (DoubleDecl(fName,ln))
      | None -> ()
      );

      let argTypes = check_args args ln fName in 
      let sgnt = fType::argTypes in 
      (match find_opt decl fName with 
      | Some(_) -> 
          raise (DoubleDecl(fName,ln))
      | None -> 
        add decl fName sgnt; 
        ()
      )
(*///////////////////////////////////////////////////*)
  | GVarDef(vType,vName,vVal, ln) -> 
      (match find_opt structs vName with
      | Some(_) -> raise (DoubleDecl(vName,ln))
      | None -> ()
      );

      (match find_opt decl vName with 
      | Some(foundType) ->( 
        if [vType] = foundType then ()
        else 
          raise (TypeMismatch(vName,ln))
        )
      | None -> ()
      );

      (match find_opt def vName with
      | Some(_) -> 
        raise (DoubleDecl(vName,ln))
      | None -> 
        let hash =create 0 in
        let valType = type_check_expr hash vVal in 
        if cmp vType valType then(
          add def vName [vType]; 
          ()
        )else(
          raise (TypeMismatch(vName,ln))
        )
      )
(*///////////////////////////////////////////////////*)
  | GVarDecl(vType, vName, ln) ->
      (match find_opt structs vName with
      | Some(_) -> raise (DoubleDecl(vName,ln))
      | None -> ()
      ); 

      (match find_opt def vName with 
        | Some(foundType) ->( 
          if [vType] = foundType then ()
          else 
            raise (TypeMismatch(vName,ln))
          )
        | None -> ()
        );

      (match find_opt decl vName with
      | Some(_) -> 
        raise (DoubleDecl(vName,ln))
      | None -> 
          add def vName [vType]; 
      )
      
    
(*///////////////////////////////////////////////////*)
(*in cigrid structs can be empty, but cannot be redeclared*)
  | Gstruct(sName, args, ln) -> 
      (match find_opt decl sName with
      | Some(_) -> raise (DoubleDecl(sName,ln))
      | None -> ()
      );
      (match find_opt def sName with
      | Some(_) -> raise (DoubleDecl(sName,ln))
      | None -> ()
      );

      (match find_opt structs sName with 
      | Some(_) -> 
        raise (DoubleDecl(sName,ln))
      | None -> 
        ignore(check_args args ln sName);
        add structs sName args;
        () 
      )

let type_check_program = function
  | Prog(globalList) -> List.iter (type_check_global) globalList

(*this does both name and type checking, I don't think it is necessarry
to be able to do name checks without type-checks*)
let check_type ast =
  (try type_check_program ast with
  | DoubleDecl (s, n) -> 
      printf "Error, double decl of %s\n" s;
      eprintf "%d\n" n; exit 2
  | UndeclaredVariable (s,n) -> 
    printf "Error, undeclared variable %s\n" s; 
    eprintf "%d\n" n;exit 2
  | UndeclaredFunction (s,n) -> 
    printf "Error, undeclared function %s\n" s; 
    eprintf "%d\n" n; exit 2
  | UndeclaredStruct (s,n) -> 
    printf "Error, undeclared struct %s\n" s; 
    eprintf "%d\n" n;  exit 2
  | TypeMismatch (s,n) -> 
    printf "Error, type mismatch of %s\n" s; 
    eprintf "%d\n" n;exit 2
  | IndexTypeMismatch (s,n) -> 
    printf "Error, Index is not int %s\n" s; 
    eprintf "%d\n" n;exit 2
  | ConditionMismatch (n) ->
    printf "Error, Condition is type void \n"; 
    eprintf "%d\n" n; exit 2
  | IllegalPointerOperation (n) ->
    printf "Error, Illegal pointer operation \n"; 
    eprintf "%d\n" n; exit 2
  | NonexistentType (s,n) ->
    printf "Error, type not declared %s\n" s; 
    eprintf "%d\n" n;exit 2
    
  ); ()