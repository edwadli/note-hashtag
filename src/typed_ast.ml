
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

let rec find_field types t access_list =
  (* make sure we are accessing a Type() *)
  let tname = match t with
    | Ast.Type(tname) -> tname
    | _ -> failwith ("cannot field access type "^Ast.string_of_type t)
  in
  (* make sure type exists *)
  let TDefault(_, fields) =
    match List.find types ~f:(fun (TDefault(s,_)) -> s = tname) with
      | None -> failwith ("Internal Error: Could not find type "^tname)
      | Some(x) -> x
  in match access_list with
    | [] -> failwith "Internal Error: tried field access with empty list"
    | field ::tail -> let (n,(x,t)) = match List.find fields ~f:(fun (s,_) -> s = field) with
                        | None -> failwith ("field "^field^" not found in type "^tname)
                        | Some(x) -> x
                      in match tail with
                        | [] -> (n,(x,t))
                        | _ -> find_field types t tail



let unique_add l item = 
  if List.mem !l item
    then l := !l
    else l := item :: !l

let rec find_variable (scope: symbol_table) name =
  match List.find scope.variables ~f:(fun (s, _) -> s = name) with
  | None -> begin
    match scope.parent with
      | Some(parent) -> find_variable parent name
      | None -> raise Not_found
    end
  | Some(x) -> x

let find_function functions name num_args = match
  List.find functions ~f:(function (n, Ast.FunDef(fname,_,_)) -> name = fname && num_args = n)
  with
    | Some(x) -> Log.debug "Found %s as nh function" name; x
    | None ->
      Log.debug "Couldn't find %s in nh functions" name;
      failwith ("Function " ^ name ^ " can't be called with these arguments")

let find_extern externs name arg_types =
  match List.find externs
    ~f:(fun (Ast.ExternFunDecl(_, _, _, fname, ftypes, _)) -> fname = name && ftypes = arg_types) with
  | Some(x) -> Log.debug "Found %s as extern function" name; x
  | None ->
    Log.debug "Couldn't find %s as extern function" name;
    failwith ("Function " ^ name ^ " can't be called with these arguments")

let is_nh_function env name =
  List.exists env.functions ~f:(fun (_, Ast.FunDef(fname, _, _)) -> fname = name)

let is_cpp_function env name =
  List.exists env.extern_functions ~f:(fun (Ast.ExternFunDecl(_, _, _, fname, _, _)) -> fname = name)

(* Make sure functions are unique with each other and externs *)
let check_unique_functions fundefs externs =
  List.fold_left fundefs ~init:[]
  ~f:(fun defs astfundef ->
    let Ast.FunDef(fname, args, expr) = astfundef in
    let nfundef = (List.length args, Ast.FunDef(fname, args, expr)) in
    if List.exists externs
      ~f:(fun (Ast.ExternFunDecl(_, _, _, nh_name, arg_types, _)) ->
        nh_name = fname && List.length arg_types = List.length args)
    then failwith ("Function " ^ fname ^ " is already declared as an external function")
    else
      if List.mem defs nfundef
      then failwith ("Function " ^ fname ^ " is already defined")
      else nfundef :: defs
  )

let rec sast_expr env tfuns_ref = function
  | Ast.LitBool(x) -> Sast.LitBool(x), Ast.Bool
  | Ast.LitInt(x) -> Sast.LitInt(x), Ast.Int
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
  
  | Ast.FunApply(name, arg_exprs) ->
    (* Common code for all function calls *)
    (* get typed versions of input expressions *)
    let arg_texprs = List.map arg_exprs ~f:(sast_expr env tfuns_ref) in
    (* get types of the typed arguments *)
    let arg_types = List.map arg_texprs ~f:(fun (_, t) -> t) in
    
    (* NH function calls *)
    if is_nh_function env name
    then
      (* find the function *)
      let num_args = List.length arg_exprs in
      let (_,Ast.FunDef(_,params,expr)) = find_function env.functions name num_args in
      (* zip params with input types *)
      let tparams = match List.zip params arg_types with
        | None -> failwith "Internal error: Mismatched lengths of types and arguments while type checking function call"
        | Some(x) -> x
      in
      (* check if types of inputs can be used with this function and UPDATE tfuns_ref *)
      let (sexpr, t) = try check_function_type tparams expr tfuns_ref env
        with Failure(reason) ->
          Log.info "Function template type check failed. Inner exception: %s" reason;
          failwith ("Incorrect types passed into function "^name)
      in begin
      (* UPDATE tfuns_ref and return the sast node *)
      ignore(unique_add tfuns_ref (Sast.FunDef(name, tparams, (sexpr, t))));
      Sast.FunApply(NhFunction(name), arg_texprs), t
      end
    
    (* C++ function calls *)
    else
      if is_cpp_function env name
      then
        (* find the function *)
        let decl = find_extern env.extern_functions name arg_types in
        let Ast.ExternFunDecl(cpp_file, cpp_ns, cpp_name, _, _, ret_type) = decl in
        (* If we got here, the function is OK *)
        Sast.FunApply(CppFunction(cpp_file, cpp_ns, cpp_name), arg_texprs), ret_type
      
      (* Function name doesn't exist *)
      else failwith ("There is no function named " ^ name)
  
  | Block(exprs) ->
    let texprs = List.rev (List.fold_left exprs ~init:[]
      ~f:(fun texprs expr ->
        (* propagate any env changes within block (due to new var initialization) *)
        let env =
          match texprs with
            | [] -> env
            | head::_ -> begin match head with
                | Sast.Init(name, (_, t)), _ ->
                    let new_vars = (name, t) :: env.scope.variables in
                    let new_scope = { parent=env.scope.parent; variables=new_vars } in
                    { scope=new_scope; functions=env.functions;
                      extern_functions=env.extern_functions; types=env.types; }
                | _ -> env
              end
        in let texpr = sast_expr env tfuns_ref expr in
        texpr :: texprs
      ))
    in
    begin match List.last texprs with
      | Some(_, t) -> Block(texprs), t
      | None -> LitUnit, Ast.Unit
    end
    
  | VarRef(names) -> begin match names with
    [] -> failwith "Internal error: VarRef(string list) had empty string list"
    | name :: fields -> let (_,t) = try find_variable env.scope name
                          with Not_found -> failwith ("Var "^name^" referenced before initalization.")
                        in begin match fields with
                          [] -> VarRef(names), t
                          | _ -> let (_,(_,t)) = find_field env.types t fields
                                  in VarRef(names), t
                          end
    end

  
  | ArrIdx(name, idx) ->
    ignore (name, idx); failwith "Type checking not implemented for ArrIdx"
  
  | Arr(exprs) ->
    ignore exprs; failwith "Type checking not implemented for Arr"
  
  | ArrMusic(exprs) ->
    ignore exprs; failwith "Type checking not implemented for ArrMusic"
  
  | Conditional(condition, case_true, case_false) ->
    ignore (condition, case_true, case_false); failwith "Type checking not implemented for Conditional"
  
  | For(loop_var_name, items, body) ->
    ignore (loop_var_name, items, body); failwith "Type checking not implemented for For"
  
  | Throw(retval, msg) ->
    ignore (retval, msg); failwith "Type checking not implemented for Throw"
  
  | Assign(names, expr) ->
      let (value, tvalue) = sast_expr env tfuns_ref expr in
      begin match names with
        | [] -> failwith "Internal error: Assign(names, _) had empty string list"
        | name :: fields -> try begin
                            let (_,t) = find_variable env.scope name in
                            match fields with
                              | [] -> if t = tvalue
                                        then Assign(names, (value, tvalue)), Ast.Unit
                                        else failwith ("cannot assign "^Ast.string_of_type tvalue^
                                          " to var of type "^Ast.string_of_type t) 
                              | _ -> let (_,(_,t)) = find_field env.types t fields in
                                      if t = tvalue
                                        then Assign(names, (value, tvalue)), Ast.Unit
                                      else failwith ("cannot assign "^Ast.string_of_type tvalue^
                                          " to field of type "^Ast.string_of_type t)
                            end with Not_found -> match fields with
                              | [] -> Init(name, (value, tvalue)), Ast.Unit
                              | _ -> failwith ("Cannot assign to fields of uninitialized var "^name)
      end
  
  | StructInit(typename, init_list) ->
    ignore (typename, init_list); failwith "Type checking not implemented for StructInit"

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
      let Ast.ExternFunDecl(_, _, _, _, arg_types, ret_type) = item in
      (* Find out whether the user-defined types, if any, are valid *)
      if List.for_all (ret_type :: arg_types) ~f:(function
        | Ast.Array(Ast.Type(name)) | Ast.Type(name) ->
          List.exists env_types ~f:(fun (type_name, _) -> type_name = name)
        | _ -> true (* built-in types are always allowed *)
      )
      then
        (* Only add new things *)
        if List.mem validated item
        then failwith ("External function has already been declared:\n" ^ Ast.string_of_extern item)
        else item :: validated
      else failwith ("Invalid type in external function declaration:\n" ^ Ast.string_of_extern item)
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
  let sexpr = sast_expr env tfuns_ref (Ast.Block(exprs)) in
  let cpp_includes = List.dedup (List.map externs ~f:(fun (ExternFunDecl(header, _, _, _, _, _)) -> header)) in
  cpp_includes, !tfuns_ref, [sexpr], env.types
