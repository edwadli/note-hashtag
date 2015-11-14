
open Core.Std
open Sast

(* environment *)
type symbol_table = {
  parent: symbol_table option;
  variables: (string * t) list;
}

type environment = {
  scope: symbol_table; (* vars symbol table *)
  functions: (int * Ast.fundef) list; (* (num args * fundef) list *)
  types: Sast.tdefault list;
}

let unique_add l item = 
  if List.mem !l item
    then l := !l
    else l := item :: !l

let rec find_variable (scope: symbol_table) name =
  try
    List.find scope.variables ~f:(fun (s, _) -> s = name)
  with Not_found ->
    match scope.parent with
      | Some(parent) -> find_variable parent name
      | None -> raise Not_found

let find_function functions name num_args = match
  List.find functions ~f:(function (n, Ast.FunDef(fname,_,_)) -> name = fname && num_args = n)
  with
    | Some(x) -> x
    | None -> failwith ("undeclared function "^name)

let check_unique fundefs = List.fold_left fundefs ~init:[]
  ~f:(fun defs astfundef
    ->  let Ast.FunDef(fname, args, expr) = astfundef in
        let nfundef = (List.length args, Ast.FunDef(fname, args, expr)) in
        if List.mem defs nfundef
          then failwith ("Function "^fname^"already defined")
          else nfundef :: defs)




let rec sast_expr env tfuns_ref = function
  | Ast.LitBool(x) -> Sast.LitBool(x), Sast.Int
  | Ast.LitInt(x) -> Sast.LitInt(x), Sast.Bool
  | Ast.LitFloat(x) -> Sast.LitFloat(x), Sast.Float
  | Ast.LitStr(x) -> Sast.LitStr(x), Sast.String
  | Ast.Binop(lexpr, op, rexpr) ->
    let lexprt = sast_expr env tfuns_ref lexpr in
    let rexprt = sast_expr env tfuns_ref rexpr in
    let (_, lt) = lexprt in
    let (_, rt) = rexprt in
    begin match op with
      | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div
      | Ast.Lt | Ast.Lte when (lt = rt && (lt = Sast.Float || lt = Sast.Int)) -> Sast.Binop(lexprt,op,rexprt), lt
      | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div
      | Ast.Lt | Ast.Lte ->
        failwith "This operation is only defined for float and int"
      
      | Ast.Mod when (lt = rt && lt = Sast.Int) -> Sast.Binop(lexprt,op,rexprt), lt
      | Ast.Mod -> failwith "This operation is only defined for int"

      | Ast.Eq | Ast.Neq when lt = rt -> Sast.Binop(lexprt,op,rexprt), lt
      | Ast.Eq | Ast.Neq -> failwith "left and right side expressions must be of same type"
      
      | Ast.And | Ast.Or when (lt = rt && lt = Sast.Bool) -> Sast.Binop(lexprt,op,rexprt), lt
      | Ast.And | Ast.Or -> failwith "This operation is only defined for bool"

      | Ast.Concat -> begin match lt, rt with
        (* disallow chords to be concatted *)
        | Sast.Array(l), Sast.Array(r)
          when l = r && l <> Sast.Type("pitch") ->
          Sast.Binop(lexprt,op,rexprt), lt
        (* note that track is a type, not array *)
        | Sast.Type("track"), Sast.Type("track")
          -> Sast.Binop(lexprt,op,rexprt), lt
        | _ -> failwith "This operation is only defined for same nonprimitive types" end
      | Ast.Chord -> begin match lt, rt with
        (* chordOp can be with pitch or chord or int *)
        | Sast.Array(Sast.Type("pitch")), Sast.Array(Sast.Type("pitch"))
        | Sast.Type("pitch"), Sast.Array(Sast.Type("pitch"))
        | Sast.Array(Sast.Type("pitch")), Sast.Type("pitch")
        | Sast.Type("pitch"), Sast.Type("pitch")
        | Sast.Int, Sast.Array(Sast.Type("pitch"))
        | Sast.Array(Sast.Type("pitch")), Sast.Int
        | Sast.Int, Sast.Type("pitch")
        | Sast.Type("pitch"), Sast.Int
        | Sast.Int, Sast.Int
          -> Sast.Binop(lexprt,op,rexprt), Sast.Array(Sast.Type("pitch"))
        | _ -> failwith "This operation is only defined for pitch, chord, or int" end
      | Ast.Octave -> begin match lt, rt with
        | Sast.Type("pitch"), Sast.Int
        | Sast.Int, Sast.Int -> Sast.Binop(lexprt,op,rexprt), Sast.Type("pitch")
        | _ -> failwith "This operation is only defined for [pitch int] and int" end
      | Ast.Zip -> begin match lt, rt with
        (* zip works with music arr, chord, pitch, or int/float *)
        | Sast.Float, Sast.Int
        | Sast.Float, Sast.Type("pitch")
        | Sast.Float, Sast.Array(Sast.Type("pitch"))
        | Sast.Float, Sast.Array(Sast.Array(Sast.Type("pitch")))
        | Sast.Array(Sast.Float), Sast.Int
        | Sast.Array(Sast.Float), Sast.Type("pitch")
        | Sast.Array(Sast.Float), Sast.Array(Sast.Type("pitch"))
        | Sast.Array(Sast.Float), Sast.Array(Sast.Array(Sast.Type("pitch")))
          -> Sast.Binop(lexprt,op,rexprt), Sast.Type("track")
        | _ -> failwith "Incorrect types for zip" end
    end
  | Ast.Uniop(op, expr) ->
    let exprt = sast_expr env tfuns_ref expr in
    let (_, t) = exprt in
    begin match op with
      | Ast.Not when t = Sast.Bool -> Sast.Uniop(op, exprt), t
      | Ast.Not -> failwith "This operator is only defined for bool"

      | Ast.Neg when t = Sast.Int || t = Sast.Float -> Sast.Uniop(op, exprt), t
      | Ast.Neg -> failwith "This operator is only defined for int or float"
      
      | Ast.Sharp | Ast.Flat when t = Sast.Int || t = Sast.Type("pitch")
        -> Sast.Uniop(op, exprt), Sast.Type("pitch")
      |Ast.Sharp | Ast.Flat -> failwith "This operator is only defined for int or pitch"
    end
  | Ast.FunApply(name, expr_list) ->
    (* find the function *)
    let num_args = List.length expr_list in
    let (_,Ast.FunDef(_,params,expr)) = find_function env.functions name num_args in
    (* get types of input expressions *)
    let texpr_list = List.map expr_list ~f:(sast_expr env tfuns_ref) in
    (* zip params with input types *)
    let types_of_input = List.map texpr_list ~f:(fun (_,t) -> t) in
    let tparams = match List.zip params types_of_input with
      | None -> failwith "Should never happen, zip failed"
      | Some(x) -> x
    in
    (* check if types of inputs can be used with this function and UPDATE tfuns_ref *)
    let (sexpr, t) = try check_function_type tparams expr tfuns_ref env
      with _ -> failwith ("Incorrect types passed into function "^name)
    in begin
    (* UPDATE tfuns_ref and return the sast node *)
    ignore(unique_add tfuns_ref (Sast.FunDef(name, tparams, (sexpr, t)))); (Sast.FunApply(name, texpr_list), t)
    end

  | _ -> failwith "VarRef, ArrIdx, Arr, ArrMusic, Block, Conditional, For,\
      Throw, Assign, StructInit not implemented"
(*   | Ast.VarRef(base::access) ->
    let (_, t) = try find_variable env.scope base with Not_found ->
      failwith ("undeclared identifier " ^ base) in
    if List.length access = 0 then Sast.VarRef( VarName(base::access) ), t
      else  *)

and check_function_type tparams expr tfuns_ref env = 
  let env = { scope={ variables=tparams; parent=env.scope.parent }; functions=env.functions; types=env.types } in
  sast_expr env tfuns_ref expr

and typed_typedefs typedefs =
  (* TODO: actually return list of Sast.tdefault *)
  []
(* Mutually recursive types NO!!!!!!!!!!! *)


(* Note that includes have been processed and merged into exprs by this point *)
let sast_of_ast (fundefs, exprs, typedefs) = 
  (* temporarily ignore includes -> NO GLOBALS YET *)
  let globals = {variables=[]; parent=None} in
  (* temporarily ignore typedefs *)
  let tdefaults = typed_typedefs typedefs in
  (* make sure fundefs are unique *)
  let nfundefs = check_unique fundefs in
  let env = { scope={variables=[]; parent=Some(globals)}; functions=nfundefs; types=tdefaults } in
  let tfuns_ref = ref [] in
  let sexprs = List.map exprs ~f:(sast_expr env tfuns_ref) in
  (!tfuns_ref ,sexprs , env.types )

