open Core.Std
open Sast

(* environment *)
type symbol_table = {
  parent: symbol_table option;
  variables: (string * Ast.t) list;
}

type environment = {
  scope: symbol_table; (* vars symbol table *)
  functions: (int * Ast.fundef) list; (* (num args * fundef) list *)
  extern_functions: Ast.externfun list;
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
    | None -> failwith ("undeclared template " ^ name)

let check_unique_functions fundefs externs =
  List.fold_left fundefs ~init:[]
  ~f:(fun defs astfundef ->
    let Ast.FunDef(fname, args, expr) = astfundef in
    let nfundef = (List.length args, Ast.FunDef(fname, args, expr)) in
    if List.exists externs
      ~f:(fun e ->
        let Ast.ExternFunDecl(_, _, _, nh_name, types) = e in
        nh_name = fname && List.length types = List.length args)
    then failwith ("Function " ^ fname ^ " is already declared as an external function")
    else
      if List.mem defs nfundef
      then failwith ("Function " ^ fname ^ " is already defined")
      else nfundef :: defs
  )

let rec sast_expr env tfuns_ref = function
  | Ast.LitBool(x) -> Sast.LitBool(x), Ast.Int
  | Ast.LitInt(x) -> Sast.LitInt(x), Ast.Bool
  | Ast.LitFloat(x) -> Sast.LitFloat(x), Ast.Float
  | Ast.LitStr(x) -> Sast.LitStr(x), Ast.String
  | Ast.Binop(lexpr, op, rexpr) ->
    let lexprt = sast_expr env tfuns_ref lexpr in
    let rexprt = sast_expr env tfuns_ref rexpr in
    let (_, lt) = lexprt in
    let (_, rt) = rexprt in
    begin match op with
      | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div
      | Ast.Lt | Ast.Lte when (lt = rt && (lt = Ast.Float || lt = Ast.Int)) -> Sast.Binop(lexprt,op,rexprt), lt
      | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div
      | Ast.Lt | Ast.Lte ->
        failwith "This operation is only defined for float and int"
      
      | Ast.Mod when (lt = rt && lt = Ast.Int) -> Sast.Binop(lexprt,op,rexprt), lt
      | Ast.Mod -> failwith "This operation is only defined for int"

      | Ast.Eq | Ast.Neq when lt = rt -> Sast.Binop(lexprt,op,rexprt), lt
      | Ast.Eq | Ast.Neq -> failwith "left and right side expressions must be of same type"
      
      | Ast.And | Ast.Or when (lt = rt && lt = Ast.Bool) -> Sast.Binop(lexprt,op,rexprt), lt
      | Ast.And | Ast.Or -> failwith "This operation is only defined for bool"

      | Ast.Concat -> begin match lt, rt with
        (* disallow chords to be concatted *)
        | Ast.Array(l), Ast.Array(r)
          when l = r && l <> Ast.Type("pitch") ->
          Sast.Binop(lexprt,op,rexprt), lt
        (* note that track is a type, not array *)
        | Ast.Type("track"), Ast.Type("track")
          -> Sast.Binop(lexprt,op,rexprt), lt
        | _ -> failwith "This operation is only defined for same nonprimitive types" end
      | Ast.Chord -> begin match lt, rt with
        (* chordOp can be with pitch or chord or int *)
        | Ast.Array(Ast.Type("pitch")), Ast.Array(Ast.Type("pitch"))
        | Ast.Type("pitch"), Ast.Array(Ast.Type("pitch"))
        | Ast.Array(Ast.Type("pitch")), Ast.Type("pitch")
        | Ast.Type("pitch"), Ast.Type("pitch")
        | Ast.Int, Ast.Array(Ast.Type("pitch"))
        | Ast.Array(Ast.Type("pitch")), Ast.Int
        | Ast.Int, Ast.Type("pitch")
        | Ast.Type("pitch"), Ast.Int
        | Ast.Int, Ast.Int
          -> Sast.Binop(lexprt,op,rexprt), Ast.Array(Ast.Type("pitch"))
        | _ -> failwith "This operation is only defined for pitch, chord, or int" end
      | Ast.Octave -> begin match lt, rt with
        | Ast.Type("pitch"), Ast.Int
        | Ast.Int, Ast.Int -> Sast.Binop(lexprt,op,rexprt), Ast.Type("pitch")
        | _ -> failwith "This operation is only defined for [pitch int] and int" end
      | Ast.Zip -> begin match lt, rt with
        (* zip works with music arr, chord, pitch, or int/float *)
        | Ast.Float, Ast.Int
        | Ast.Float, Ast.Type("pitch")
        | Ast.Float, Ast.Array(Ast.Type("pitch"))
        | Ast.Float, Ast.Array(Ast.Array(Ast.Type("pitch")))
        | Ast.Array(Ast.Float), Ast.Int
        | Ast.Array(Ast.Float), Ast.Type("pitch")
        | Ast.Array(Ast.Float), Ast.Array(Ast.Type("pitch"))
        | Ast.Array(Ast.Float), Ast.Array(Ast.Array(Ast.Type("pitch")))
          -> Sast.Binop(lexprt,op,rexprt), Ast.Type("track")
        | _ -> failwith "Incorrect types for zip" end
    end
  | Ast.Uniop(op, expr) ->
    let exprt = sast_expr env tfuns_ref expr in
    let (_, t) = exprt in
    begin match op with
      | Ast.Not when t = Ast.Bool -> Sast.Uniop(op, exprt), t
      | Ast.Not -> failwith "This operator is only defined for bool"

      | Ast.Neg when t = Ast.Int || t = Ast.Float -> Sast.Uniop(op, exprt), t
      | Ast.Neg -> failwith "This operator is only defined for int or float"
      
      | Ast.Sharp | Ast.Flat when t = Ast.Int || t = Ast.Type("pitch")
        -> Sast.Uniop(op, exprt), Ast.Type("pitch")
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
 |Ast.Arr(expr_list) ->
    let exprt = List.map expr_list ~f:(sast_expr env tfuns_ref) in
    let rec make_t l =  match l with
	|(_, t) :: rest -> t :: make_t rest
	|[] -> []
    in let t_list = make_t exprt in
    let t = List.nth_exn t_list 0 in
    let same_type = List.for_all t_list ~f:(fun x ->
	x = t) in
    (if same_type then
    	Sast.Arr(exprt), t
    else
	failwith("Different types found in list"))
 |Ast.ArrMusic(expr_list) ->
    let exprt = List.map expr_list ~f:(sast_expr env tfuns_ref) in
    let rec make_t l =  match l with
	|(_, t) :: rest -> t :: make_t rest
	|[] -> []
    in let t_list = make_t exprt in
    let t = List.nth_exn t_list 0 in
    let same_type = List.for_all t_list ~f:(fun x ->
	x = t) in
    (if same_type then
    	Sast.ArrMusic(exprt), t
    else
	failwith("Different types found in list"))
 |Ast.EmptyList(t) ->
    Sast.EmptyList(t), t
 |Ast.EmptyMusicList(t) ->
    Sast.EmptyMusicList(t), t 
  | _ -> failwith "VarRef, ArrIdx, Arr, ArrMusic, Block, Conditional, For,\
      Throw, Assign, StructInit not implemented"
(*   | Ast.VarRef(base::access) ->
    let (_, t) = try find_variable env.scope base with Not_found ->
      failwith ("undeclared identifier " ^ base) in
    if List.length access = 0 then Sast.VarRef( VarName(base::access) ), t
      else  *)

and check_function_type tparams expr tfuns_ref env = 
  let env' = {
    scope = { variables = tparams; parent = env.scope.parent };
    functions = env.functions;
    extern_functions = env.extern_functions;
    types = env.types;
  } in
  sast_expr env' tfuns_ref expr

and typed_typedefs typedefs =
  (* TODO: actually return list of Sast.tdefault *)
  ignore (typedefs); []
(* Mutually recursive types NO!!!!!!!!!!! *)

and typed_externs externfuns env_types =
  List.fold_left externfuns ~init:[]
    ~f:(fun validated item ->
      let Ast.ExternFunDecl(_, _, _, _, arg_types) = item in
      (* Find out whether the user-defined types, if any, are valid *)
      let arg_types_valid = List.for_all arg_types ~f:(function
        | Ast.Type(name) -> List.exists env_types ~f:(fun (type_name, _) -> type_name = name)
        | _ -> true (* all other types are OK *)
      ) in
      if arg_types_valid && List.mem validated item
        then failwith ("External function has already been declared:\n" ^ Ast.string_of_extern item)
        else item :: validated
    )

(* Note that includes have been processed and merged into exprs by this point *)
let sast_of_ast (fundefs, externs, exprs, typedefs) = 
  (* temporarily ignore includes -> NO GLOBALS YET *)
  let globals = {variables=[]; parent=None} in
  (* temporarily ignore typedefs *)
  let tdefaults = typed_typedefs typedefs in
  (* make sure fundefs are unique *)
  let externs = typed_externs externs tdefaults in
  let nfundefs = check_unique_functions fundefs externs in
  let env = {
    scope = { variables=[]; parent=Some(globals) };
    functions = nfundefs;
    extern_functions = externs;
    types = tdefaults;
  } in
  let tfuns_ref = ref [] in
  let sexprs = List.map exprs ~f:(sast_expr env tfuns_ref) in
  (!tfuns_ref ,sexprs , env.types )
