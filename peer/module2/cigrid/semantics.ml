open Ast
open Pprinter
open Printf

type func_sig = identifier*type_id*(type_id list) (* name, return type, parameter types*)
type var_sig = identifier*type_id
type scope = 
  | Global
  | Nested of var_sig list * scope (* reference to parent scope *)
type environment = {
  declared_global_vars : var_sig list ; 
  defined_global_vars : var_sig list ; 
  declared_functions : func_sig list ; 
  defined_functions : func_sig list ;
  declared_structs : (identifier * ((identifier*type_id) list)) list; (* struct name and list of fields*)
  global_identifiers : identifier list;
  variable_scope : scope ; 
  func_ret_type : type_id option
}


type analysis_result = 
  | AnalysisSuccess
  | AnalysisNameError of int
  | AnalysisTypeError of int



let clean_params pList = 
  let rec work lst acc = 
  match lst with 
  | [] -> acc 
  | (meta, Param((_,t), _))::tail -> work tail (t::acc) (* only need to store the type *)
  in
  work pList []

let clean_fields fList = 
  let rec work lst acc = 
  match lst with 
  | [] -> acc 
  | (_, Field((_,t), (_,f)))::tail -> work tail ((f,t)::acc) (* only need to store the name and type *)
  in
  work fList []

let make_func_sig func_name func_ret parameters = (func_name, func_ret, clean_params parameters)

(* check for type equality while ignoring metadata*)
let rec type_equality t1 t2 = match t1, t2 with 
  | TPoint((_,a)), TPoint((_,b)) -> type_equality a b 
  | Identifier((_,a)), Identifier((_,b)) -> a = b 
  | a, b -> a = b 

(* used to check if function is declared/defined while ignoring metadata *)
let check_if_func_in_list fSig lst = 
  let (fName, fRet, fParams) = fSig in 
  let rec param_equality acc (p1, p2) = 
    acc && (type_equality p1 p2) in
  let rec work funcs = match funcs with 
  | [] -> false 
  | (func_name, func_ret, params)::tail -> 
    if (func_name = fName) &&(type_equality fRet func_ret)&&(List.fold_left param_equality true (List.combine params fParams))
      then true else work tail in
  work lst

let check_func_defined fSig env =
  check_if_func_in_list fSig env.defined_functions 

let check_func_declared fSig env =
  check_if_func_in_list fSig env.declared_functions
   
(* used to check if var is declared/defined globally while ignoring metadata*)
let check_if_var_in_list vSig lst = 
  let (vName, vType) = vSig in 
  let rec work vars = match vars with 
  | [] -> false 
  | (var_name, var_type)::tail -> 
    if (var_name = vName) &&(type_equality var_type vType)
      then true else work tail in
  work lst

let check_var_defined vSig env =
  check_if_var_in_list vSig env.defined_global_vars 
let check_var_declared vSig env =
  check_if_var_in_list vSig env.declared_global_vars 

(* checks through all scopes from the inner most and out to global to find the type of a variable *)  
let get_var_type vName env = 
    let rec iterateVars vName vList = match vList with 
      | [] -> None
      | (hN, hT)::tail -> if hN = vName then Some(hT) else iterateVars vName tail in 
    let rec iterateScopes vName scopes = match scopes with 
    | Global -> iterateVars vName env.declared_global_vars
    | Nested(vList, parent) -> match iterateVars vName vList with 
    | None -> iterateScopes vName parent 
    | Some(t) -> Some(t)
    in 
    iterateScopes vName env.variable_scope

(* checks if the variable vName has already been declared in the inner most scope*)
let var_declared_in_scope vName env = 
  let rec iterateVars vName vList = match vList with 
    | [] -> false
    | (hN, hT)::tail -> if hN == vName then true else iterateVars vName tail in 
  match env.variable_scope with 
  | Global -> exit 21 (* since this is only called from inside a function this never happens *)
  | Nested(vList, _) -> iterateVars vName vList

let type_is_numeric t = match t with 
  | TInt | TChar -> true 
  | _ -> false

  (* an Int can be assigned to a pointer, chars and ints cast implicitly, pointer types are compatible if they point to the same type, and structs are compatible if they have the same name. all other types are incompatible. *)
let rec check_type_compatible t1 t2 = if t1=t2 then true else match t1,t2 with
  | TPoint(_),TInt|TInt,TChar|TChar,TInt -> true 
  | TPoint((_,a)), TPoint((_,b)) -> check_type_compatible a b
  | Identifier((_,a)), Identifier((_,b)) -> a = b 
  | a,b ->  false

let struct_has_field structId fieldId env = 
  let rec iterateFields fieldId fieldList = match fieldList with 
  | [] -> None 
  | (name, fieldType)::tail -> if fieldId = name then Some(fieldType) else iterateFields fieldId tail in 
  let rec iterateStructs structId fieldId structList = match structList with 
    | [] -> None
    | (name, fields)::tail -> if name <> structId then iterateStructs structId fieldId tail else iterateFields fieldId fields in
  iterateStructs structId fieldId env.declared_structs

(* lookup function for calling purposes *)
let find_func funcId env = 
  let rec work funcId funcList = match funcList with 
    | [] -> None 
    | (fName, retType, params)::tail -> if fName = funcId then Some(retType, params) else 
                                        work funcId tail in 
  work funcId env.declared_functions

(* pointers can check equality with anything and nothing else, void is not a valid type, and structs have no operations *)
let check_bin_op_result_type binOp arg1 arg2 = match binOp, arg1, arg2 with 
      | EqOp, TPoint(_),_|EqOp,_,TPoint(_)| NeqOp, TPoint(_),_|NeqOp,_,TPoint(_) -> Some(TInt)
      | _, TPoint(_), _|_,_,TPoint(_) -> None
      | _, TVoid, _ | _,_,TVoid -> None 
      | _, Identifier(_), _ | _,_,Identifier(_) -> None 
      | _,_,_ -> Some(TInt) 

let rec check_type_exists typeId env = match typeId with 
    | TPoint((_, t)) -> check_type_exists t env 
    | Identifier((_,s)) -> List.fold_left (fun acc (structName, _) -> acc || (structName = s)) false env.declared_structs
    | _ -> true (* all built in types exist *)


(* for the name analysis, check if an identifier has already been used *)
let id_in_env id env = List.mem id env 

(* -------------------------------------- static semantics -------------------------------------------*)
(* ss== static semantics *)

(* returns status, environment [not necessary], and the type of the expression*)
let rec ss_expr tree env = 
  let (meta, t) = tree in 
  match t with 
  | EVar((_,identifier)) -> 
    (match get_var_type identifier env with 
    | None -> (AnalysisTypeError(meta.line), env, TVoid) 
    | Some(tpe) -> (AnalysisSuccess, env, tpe))
  | EInt (n ) -> (AnalysisSuccess, env, TInt)
  | EChar (c ) -> (AnalysisSuccess, env, TChar)
  | EString (s ) -> (AnalysisSuccess, env, TPoint((meta,TChar)))
  | EUnOp (_,expression ) -> 
      let (res,_,resT) = ss_expr expression env in 
      ( (* only to unary operations on things that accept them *)
        match resT with 
        | TPoint(_) | Identifier(_) | TVoid -> (AnalysisTypeError(meta.line), env, TVoid)
        | _ -> (res, env, resT)
      )
  | EBinOp ((_,binOp),expr1, expr2 ) -> 
      let (res1,_,resT1) = ss_expr expr1 env in 
      if res1 <> AnalysisSuccess then (res1, env, TVoid) else 
      let (res2,_,resT2) = ss_expr expr2 env in 
      if res2 <> AnalysisSuccess then (res2, env, TVoid) else 
      (match check_bin_op_result_type binOp resT1 resT2 with 
      | None -> (AnalysisTypeError(meta.line), env, TVoid)
      | Some(t) -> (AnalysisSuccess, env, t))
  | ECall ((_,funcName),exprs ) -> 
    (match find_func funcName env with 
    | None -> (AnalysisNameError(meta.line), env, TVoid)
    | Some(retType, params) -> 
      let expr_results = List.map (fun exp -> ss_expr exp env) exprs in 
      (* check if any function parameter failed to evaluate correctly *)
      let (res, env2, _) = List.fold_left (fun (accR,accE,accT) (res,env,tpe) -> 
        if accR<>AnalysisSuccess ||  res == AnalysisSuccess then (accR,accE,accT) else (res,env,tpe)) (AnalysisSuccess, env, TVoid) expr_results in 
      if res <> AnalysisSuccess then (res,env2,TVoid) else 
      let parameters = List.map (fun (_,_,t) -> t) expr_results in 
      (* check if there is a missmatch between provided parameters and expected parameters *)
        (* number of parameters: *)
      if (List.length params) <> (List.length parameters) then (AnalysisTypeError(meta.line), env, TVoid) else 
        (* type of parameters: *)
      if (List.fold_left (fun acc (t1,t2) -> acc && check_type_compatible t1 t2) true (List.combine params parameters)) 
        then (AnalysisSuccess, env, retType) else (AnalysisTypeError(meta.line), env, TVoid))
  | ENew (type_name,expression ) -> 
    let (resExpr, _, resType) = ss_expr expression env in 
    (* length of array must be a number *)
    if not(type_is_numeric resType) then (AnalysisTypeError(meta.line), env, TVoid) else 
      (AnalysisSuccess, env, TPoint(type_name))
  | EArrayAccess ((_,identifier),expression, field_opt ) -> 
    let (resExpr, _, resType) = ss_expr expression env in 
    (* array index must be a number *)
    if not(type_is_numeric resType) then (AnalysisTypeError(meta.line), env, TVoid) else 
    match get_var_type identifier env with 
    | None -> (AnalysisNameError(meta.line), env, TVoid)
    (* indexed variable must be a pointer type *)
    | Some(TPoint((_,t))) -> ( 
      match field_opt, t with 
      | None,_ -> (AnalysisSuccess, env, t)
      | Some((_,field)),Identifier((_,id)) -> 
        (* check if struct or field should be accessed *)
        (match struct_has_field id field env with 
        | None -> (AnalysisNameError(meta.line), env, TVoid)
        | Some(fieldType) -> (AnalysisSuccess, env , fieldType) )
      | _,_ -> (AnalysisNameError(meta.line), env, TVoid)
      )
      | _ -> (AnalysisTypeError(meta.line), env, TVoid)


let rec ss_stmts tree env= 
  let rec ss_stmt stmt env = 
    let (meta, t) = stmt in 
    match t with
    | SExpr(expression) -> let (res,env2,_) = ss_expr expression env in (res,env2)
    | SVarAssign ((_,identifier), expression) -> 
      (match get_var_type identifier env with
      | None -> (AnalysisNameError(meta.line), env )
      | Some(varType) -> 
        let (res,env2,exprType) = ss_expr expression env in
        (* only assign compatible types *)
        if not (check_type_compatible varType exprType) then (AnalysisTypeError(meta.line), env) else 
        (match res with
        | AnalysisSuccess -> (AnalysisSuccess, env)
        | x -> (x,env2))
      ) 
    | SScope (stmts_inner) -> 
      (* nest scopes to allow redeclarations of local variables *)
      let (res,env2) = ss_stmts stmts_inner {env with variable_scope=Nested([], env.variable_scope)} in 
      (match res with
      (* return with original environment *)
      | AnalysisSuccess -> (AnalysisSuccess, env)
      | x -> (x,env2))
    | SVarDef ((_,type_name), (_,identifier), expression) -> 
      if not(check_type_exists type_name env) then (AnalysisTypeError(meta.line), env) else 
      if var_declared_in_scope identifier env then (AnalysisNameError(meta.line), env) else 
      let (res,env2,exprType) = ss_expr expression env in 
      if not(check_type_compatible type_name exprType) then (AnalysisTypeError(meta.line), env) else 
      (match res with
      | AnalysisSuccess -> (AnalysisSuccess, 
        match env.variable_scope with 
          | Global -> exit 22 (* never happens since this function is always called inside a function scope *)
          | Nested(vList, pScope) ->  {env with variable_scope=Nested((identifier, type_name)::(vList), pScope)}
        )
      | x -> (x,env2))
    | SIf (expression,statement, else_stmt_opt) -> 
      let (resEx, _,_) = ss_expr expression env in 
      if resEx <> AnalysisSuccess then (resEx, env) else 
      let (resIf,_) = ss_stmt statement {env with variable_scope=Nested([], env.variable_scope)} in  
      if resIf <> AnalysisSuccess then (resIf, env) else 
      (match else_stmt_opt with
      | None -> (AnalysisSuccess, env)
      | Some(else_branch) -> (* nest with original scope, not output of if-clause *)
      (let (resElse,_) = ss_stmt else_branch {env with variable_scope=Nested([], env.variable_scope)}
    in (resElse, env) (* return with correct environment *)
      ))
    | SWhile (expression, while_body_stmt) -> 
      let (resEx, _, _) = ss_expr expression env in 
      if resEx <> AnalysisSuccess then (resEx, env) else 
      let (resWhile,_) = ss_stmt while_body_stmt {env with variable_scope=Nested([], env.variable_scope)} in (resWhile, env) (* return with correct environment *)
    | SBreak -> (AnalysisSuccess, env)
    | SReturn(ret_expr_opt) -> (match ret_expr_opt, env.func_ret_type with
      |None,None -> (AnalysisSuccess, env)
      |None,Some(_) | Some(_),None -> (AnalysisTypeError(meta.line), env)
      |Some(r),Some(t) -> 
        let (resEx, _, t2) = ss_expr r env in 
        if check_type_compatible t t2 then (resEx, env) else (AnalysisTypeError(meta.line), env) 
        )
    | SDelete ((_,identifier)) -> 
        (match get_var_type identifier env with 
        | None -> (AnalysisNameError(meta.line), env)
        | Some(varType) -> (match varType with 
        | TPoint(_) -> (AnalysisSuccess, env)
        | _ -> (AnalysisTypeError(meta.line), env)))
    | SArrayAssign((_,identifier), index_expr, field_opt, expression) ->  
        let (resIndex, _, indexT) = ss_expr index_expr env in 
        if resIndex <> AnalysisSuccess then (resIndex, env) else 
        if not(type_is_numeric indexT) then (AnalysisTypeError(meta.line), env) else 
        let (resExpr, _, exprT) = ss_expr expression env in
        if resExpr <> AnalysisSuccess then (resExpr, env) else 
        match get_var_type identifier env with 
        | None -> (AnalysisNameError(meta.line), env) (* variable has not been declared *)
        | Some(TPoint((_,varType))) -> (
          match field_opt, varType with 
          | None,_ -> ( (* assigning to entire struct *)
            if not(check_type_compatible varType exprT) then 
              (AnalysisTypeError(meta.line), env) else (AnalysisSuccess, env)
            )
            (* assigning to struct field *)
          | Some((_,field)), Identifier((_,structId)) -> (match struct_has_field structId field env with 
              | None -> (AnalysisNameError(meta.line), env)
              | Some(fieldType) -> 
                  if not(check_type_compatible fieldType exprT) then (AnalysisTypeError(meta.line), env) else (AnalysisSuccess,env))
          | _,_ -> (AnalysisTypeError(meta.line), env)
        )
                (* indexing non-struct type *)
        | _ -> (AnalysisTypeError(meta.line), env) in 
    let rec eat_stmts lst env = match lst with 
    | [] -> (AnalysisSuccess, env)
    | h::tail -> 
      let (res, env2) = ss_stmt h env in 
      match res with 
      | AnalysisSuccess -> eat_stmts tail env2
      | x -> (x,env2) in 
    eat_stmts tree env 

let ss_global tree env =  
  (* add parameters to local scope when entering function *)
  let rec add_parameters env2 paramList params = match params with 
    | [] -> (AnalysisSuccess,env2)
    | (meta,Param((_,t),(_,p)))::tail -> 
      (* ensure unique names *)
      if id_in_env p paramList then (AnalysisNameError(meta.line), env2) else 
      if not( check_type_exists t env) then (AnalysisTypeError(meta.line), env2) else 
      add_parameters 
        (match env2.variable_scope with 
          | Global -> exit 25 (* never happens, this is only called in a local scope *)
          | Nested(vList, pScope) ->  {env2 with variable_scope=Nested((p, t)::vList, pScope)}) (p::paramList) tail
in
  let (meta,t) = tree in 
  match t with
  | GFuncDef((_,type_name),(_,identifier),parameters, stmt)  ->
    let fSig = make_func_sig identifier type_name parameters in 
    if (check_func_defined fSig env) then (AnalysisNameError(meta.line), env)
    else   
    (* check that the function name has not been used globally before EXCEPT possibly when declared*)
    if not(check_func_declared fSig env) && List.mem identifier env.global_identifiers then (AnalysisNameError(meta.line), env) else 
      (* if the function has been declared then only define it*)
    let env2 = if check_func_declared fSig env then {env with defined_functions=fSig::env.defined_functions} 
      (* otherwise also declare it and save its identifier*)
    else {env with defined_functions=fSig::env.defined_functions; declared_functions=fSig::env.declared_functions; global_identifiers=identifier::env.global_identifiers} in

    let (res,env3) = add_parameters {env2 with variable_scope=Nested([], Global)} [] parameters  in 
    if res <> AnalysisSuccess then (res,env) else 
      (* check semantis of function with the environment containing the parameters *)
    let (res2,_) = ss_stmts [stmt] {env3 with func_ret_type=(if type_name == TVoid then None else Some(type_name))} in 
    (* return the environment with the function but without the parameters *)
    (res2, env2) 
  | GFuncDecl((_,type_name),(_,identifier),parameters)  -> 
    let fSig = make_func_sig identifier type_name parameters in
    if check_func_declared fSig env then (AnalysisNameError(meta.line), env) else 
      (AnalysisSuccess, {env with declared_functions=fSig::env.declared_functions; global_identifiers=identifier::env.global_identifiers}) 
  | GVarDef((_,type_name),(_,identifier),expression) -> 
    let vSig = (identifier,type_name) in 
    if (check_var_defined vSig env) then (AnalysisNameError(meta.line), env) else
    if List.mem identifier env.global_identifiers then (AnalysisNameError(meta.line), env) else 
    let (res,_,exprT) = ss_expr expression env in 
      if not(check_type_compatible type_name exprT) then (AnalysisTypeError(meta.line),env) else 
      if check_var_declared vSig env then 
        (res, {env with defined_global_vars=vSig::env.defined_global_vars})
      else
        (res, {env with defined_global_vars=vSig::env.defined_global_vars; declared_global_vars=vSig::env.declared_global_vars; global_identifiers=identifier::env.global_identifiers})
  | GVarDecl((_,type_name),(_,identifier)) -> 
    if List.mem identifier env.global_identifiers then (AnalysisNameError(meta.line), env) else 
    let vSig = (identifier,type_name) in
      (AnalysisSuccess, {env with declared_global_vars=vSig::env.declared_global_vars; global_identifiers=identifier::env.global_identifiers}) 
  | GStruct((_,identifier), fields) ->
    if List.mem identifier env.global_identifiers then (AnalysisNameError(meta.line), env) else 
     (AnalysisSuccess, {env with declared_structs=(identifier, clean_fields fields)::env.declared_structs; global_identifiers=identifier::env.global_identifiers})


let rec static_semantics tree = 
  let rec anlze prog env = 
  match prog with 
  | GlobTerminal -> AnalysisSuccess
  | ConsGlob(_,glob, program) -> 
      let (res, env2) = ss_global glob env in
      match res with 
      | AnalysisSuccess -> anlze program env2
      | x -> x in
      (* begin analysis with initial environment *)
  anlze tree {declared_global_vars =[] ; 
      defined_global_vars=[]; 
      declared_functions =[]; 
      defined_functions =[]; 
      declared_structs =[] ;
      global_identifiers =[] ;
      variable_scope=Global ; 
      func_ret_type=None;
    }





(* ----------------- name checking -----------------------*)

(* sna==simple name checking *)

let rec sna_expr tree env = 
  let (meta, t) = tree in 
  match t with 
  | EVar((_,identifier)) -> if id_in_env identifier env then (AnalysisSuccess, env) else (AnalysisNameError(meta.line), env)
  | EInt (n ) -> (AnalysisSuccess, env)
  | EChar (c ) -> (AnalysisSuccess, env)
  | EString (s ) -> (AnalysisSuccess, env)
  | EUnOp (_,expression ) -> sna_expr expression env
  | EBinOp (_,expr1, expr2 ) -> 
    let (res1,_) = sna_expr expr1 env in 
    if res1 <> AnalysisSuccess then (res1, env) else
      sna_expr expr2 env
  | ECall (_,exprs ) -> sna_exprs exprs env
  | ENew (_,expression ) -> sna_expr expression env
  | EArrayAccess ((_,identifier),expression, _ ) -> 
    if not(id_in_env identifier env) then (AnalysisNameError(meta.line), env) else 
      sna_expr expression env
and sna_exprs lst env= 
  let rec work es env = match es with
    | [] -> (AnalysisSuccess, env)
    | h::tail -> 
      let (res1,_) = sna_expr h env in 
      if res1 <> AnalysisSuccess then (res1, env) else
      work tail env in
  work lst env


let rec sna_stmts tree env= 
  let rec sna_stmt stmt env = 
    let (meta, t) = stmt in 
    match t with
    | SExpr(expression) -> sna_expr expression env
    | SVarAssign ((_,identifier), expression) -> 
      if not (id_in_env identifier env) then (AnalysisNameError(meta.line), env) else 
      let (res,env2) = sna_expr expression env in 
      (match res with
      | AnalysisSuccess -> (AnalysisSuccess, env)
      | x -> (x,env2))
    | SScope (stmts_inner) -> 
      let (res,env2) = sna_stmts stmts_inner env in 
      (match res with
      | AnalysisSuccess -> (AnalysisSuccess, env)
      | x -> (x,env2))
    | SVarDef (_, identifier, expression) -> 
      let (res,env2) = sna_expr expression env in 
      (match res with
      | AnalysisSuccess -> (AnalysisSuccess, (snd identifier)::env)
      | x -> (x,env2))
    | SIf (expression,statement, else_stmt_opt) -> 
      let (resEx, _) = sna_expr expression env in 
      if resEx <> AnalysisSuccess then (resEx, env) else 
      let (resIf,_) = sna_stmt statement env in  
      if resIf <> AnalysisSuccess then (resIf, env) else 
      (match else_stmt_opt with
      | None -> (AnalysisSuccess, env)
      | Some(else_branch) -> 
      sna_stmt else_branch env)
    | SWhile (expression, while_body_stmt) -> 
      let (resEx, _) = sna_expr expression env in 
      if resEx <> AnalysisSuccess then (resEx, env) else 
      sna_stmt while_body_stmt env
    | SBreak -> (AnalysisSuccess, env)
    | SReturn(ret_expr_opt) -> (match ret_expr_opt with
      |None -> (AnalysisSuccess, env)
      |Some(r) -> sna_expr r env )
    | SDelete ((_,identifier)) -> if id_in_env identifier env then (AnalysisSuccess, env) else (AnalysisNameError(meta.line), env) (* Maybe want to remove the identifier from the environment? Who am I kidding? This isn't a cybersecurity course, use-after-frees are allowed! *)
    | SArrayAssign((_,identifier), index_expr, _, expression) ->  
      if not(id_in_env identifier env) then (AnalysisNameError(meta.line), env) else
        let (resEx1, _) = sna_expr index_expr env in 
        if resEx1 <> AnalysisSuccess then (resEx1, env) else 
          sna_expr expression env in 
    let rec eat_stmts lst env = match lst with 
    | [] -> (AnalysisSuccess, env)
    | h::tail -> 
      let (res, env2) = sna_stmt h env in 
      match res with 
      | AnalysisSuccess -> eat_stmts tail env2
      | x -> (x,env2) in 
    eat_stmts tree env

let sna_global tree env =  
  let rec add_parameters env2 params = match params with 
    | [] -> env2
    | (_,Param(_,p))::tail -> add_parameters ((snd p)::env2) tail
in
  let (_,t) = tree in 
  match t with
  | GFuncDef(_,identifier,parameters, stmt)  -> 
    let env2 = add_parameters env parameters in 
    let (res,_) = sna_stmts [stmt] env2 in 
    (res, env) 
  | GVarDef(_,identifier,expression) -> 
    let (res,env2) = sna_expr expression env in 
    (match res with
    | AnalysisSuccess -> (AnalysisSuccess, (snd identifier)::env)
    | x -> (x,env2))
  | GVarDecl(_,identifier) -> (AnalysisSuccess, (snd identifier)::env)
  | _ -> (AnalysisSuccess, env)


let rec simple_name_analysis tree = 
  let rec anlze prog env = 
  match prog with 
  | GlobTerminal -> AnalysisSuccess
  | ConsGlob(_,glob, program) -> 
      let (res, env2) = sna_global glob env in
      match res with 
      | AnalysisSuccess -> anlze program env2
      | x -> x in
  anlze tree []
