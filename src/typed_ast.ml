
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

let rec has_cycle_rec nodes (self, children) seen =
  match List.find !seen ~f:(fun n -> n = self) with
    | Some(_) -> true
    | None -> seen := self :: !seen; List.fold_left
        (List.map children
          ~f:(fun child -> 
            match List.find nodes ~f:(fun (t,_) -> t = child) with
              | None -> has_cycle (List.filter nodes ~f:(fun (n,_) -> not (List.mem !seen n)))
              | Some(head) -> has_cycle_rec nodes head seen
          )
        )
        ~init:(false) ~f:(||)

and has_cycle nodes = match nodes with
  | [] -> false
  | head::_ -> has_cycle_rec nodes head (ref [])

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

let chord_of sexpr =
  begin match sexpr with
    (* use function in standard library on chordable (chord, pitch, int) exprs *)
    | (_, Ast.Int) -> Sast.FunApply(Sast.NhFunction("ChordOfPitch"),
        [ Sast.FunApply(Sast.NhFunction("PitchOfInt"), [(Sast.LitInt(0),Ast.Int)]),
          Ast.Type("pitch") ])
    | (_, Ast.Type("pitch")) -> Sast.FunApply(Sast.NhFunction("ChordOfPitch"), [sexpr])
    | (expr, Ast.Type("chord")) -> expr
    | _ -> failwith "This expression is not chordable"
  end, Ast.Type("chord")

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
        (* also allow tracks to be concatted *)
        | Ast.Type("track"), Ast.Type("track") -> Sast.FunApply(NhFunction("ConcatTracks"),[lexprt;rexprt]), lt
        | Ast.Array(l), Ast.Array(r) when l = r -> Sast.Binop(lexprt,op,rexprt), lt
        | _ -> failwith "Concat is only for defined for same typed arrays and tracks" end

      | Ast.Chord ->
          (* guarantee that chord binop is between two chords *)
          Sast.Binop(chord_of lexprt, op, chord_of rexprt), Ast.Type("chord")

      | Ast.Octave ->
          let lexprt = match lt with
            | Ast.Type("pitch") -> lexprt
            | Ast.Int -> Sast.FunApply(NhFunction("PitchOfInt"), [lexprt]), Ast.Type("pitch")
            | _ -> failwith "octave only defined for pitch or int on left side"
          in if rt = Ast.Int
            then Sast.FunApply(NhFunction("AddPitchOctave"), [lexprt;rexprt]), Ast.Type("pitch")
            else failwith "octave only defined for int on right side"

      | Ast.Zip ->
          if (lt = Ast.Float || lt = Ast.Array(Ast.Float))
            then let rexprt = match rt with
                (* either chord or array of chord is valid for zip *)
                | Ast.Array(Ast.Type("chord")) -> rexprt
                | _ -> chord_of rexprt
              in Sast.Binop(lexprt,op,rexprt), Ast.Type("track")
            else failwith "left side expression of zip must of float or array of float"
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
  
  | Ast.Block(exprs) ->
    let (texprs,_) = List.fold_left exprs ~init:([],env)
      ~f:(fun (texprs, env) expr ->
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
        (texpr :: texprs, env)
      )
    in
    let texprs = List.rev texprs in
    begin match List.last texprs with
      | Some(_, t) -> Block(texprs), t
      | None -> LitUnit, Ast.Unit
    end
    
  | Ast.VarRef(names) -> begin match names with
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
  
  | Throw(msg) ->
    ignore msg; failwith "Type checking not implemented for Throw"
  
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
      let TDefault(_, defaults) = match List.find env.types ~f:(fun (TDefault(n,_)) -> typename = n) with
        | Some(x) -> x
        | None -> failwith ("type "^typename^" not found")
      in
      let fields = List.map defaults ~f:(fun (n,(_,t)) -> (n,t)) in
      let sexprs = List.map init_list ~f:(sast_expr env tfuns_ref) in
      let varname = function
        | Init(name,(_,t)),_
            when begin match List.find fields ~f:(fun (n,_) -> n = name) with
              | Some((_,tfield)) when tfield = t -> true
              | _ -> false
            end
            -> name
        | Assign(name::tail,(_,t)),_
            when List.length tail = 0 && begin match List.find fields ~f:(fun (n,_) -> n = name) with
              | Some((_,tfield)) when tfield = t -> true
              | _ -> false
            end
            -> name
        | _ -> failwith ("Only assignments of fields are allowed in type init of "^typename)
      in
      if List.contains_dup sexprs
        ~compare:(fun lsexpr rsexpr ->
          let ln = varname lsexpr and rn = varname rsexpr in
          compare ln rn
        )
        then failwith ("cannot assign fields multiple times in type init of "^typename)
      else let init_exprs = List.fold_left defaults ~init:[]
        ~f:(fun init_exprs (name, expr) ->
          (* grab default if not explicitly initalized *)
          let field_expr = function
            | Init(_,expr),_ -> expr
            | Assign(_,expr),_ -> expr
            | _ -> failwith ("Internal error: non init/assign sexpr found in type init")
          in
          match List.find sexprs ~f:(fun sexpr -> name = varname sexpr) with
            | Some(x) -> (name, field_expr x) :: init_exprs
            | None -> (name, expr) :: init_exprs
        )
      in
      Struct(typename, init_exprs), Ast.Type(typename)



and check_function_type tparams expr tfuns_ref env = 
  let env' = {
    scope = { variables = tparams; parent = env.scope.parent };
    functions = env.functions;
    extern_functions = env.extern_functions;
    types = env.types;
  } in
  sast_expr env' tfuns_ref expr

and typed_typedefs env tfuns_ref typedefs =
  let tdefaults = List.map typedefs
    ~f:(fun (Ast.TypeDef(name, exprs)) ->
      let sexprs = List.map exprs ~f:(sast_expr env tfuns_ref) in
      let fields = List.map sexprs
        ~f:(fun sexpr ->
          match sexpr with
            | Init(name, expr),_ -> (name, expr)
            | Assign(name::tail, expr),_ when List.length tail = 0 -> (name, expr)
            | _ -> failwith ("Only initialization of fields are allowed in type decl of "^name)
        )
      in
      if List.contains_dup fields
        ~compare:(fun (ln,_) (rn,_) -> compare ln rn)
        then failwith ("Cannot init fields multiple times in type decl of "^name)
      else TDefault(name, fields)
    )
  in
  (* Remove repeats ... hope the user knows what he was doing... *)
  let tdefaults = List.dedup tdefaults
    ~compare:(fun (TDefault(ln,_)) (TDefault(rn,_)) -> compare ln rn)
  in
  (* Build dependency graph *)
  let type_deps = List.map tdefaults
    ~f:(fun (TDefault(name, defaults)) ->
      let fields = List.map defaults ~f:(fun (_,(_,t)) -> t) in (Ast.Type(name), fields)
    )
  in
  if has_cycle type_deps then failwith ("No mutually recursive types allowed")
  else tdefaults

and typed_externs externfuns env_types =
  List.fold_left externfuns ~init:[]
    ~f:(fun validated item ->
      let Ast.ExternFunDecl(_, _, _, _, arg_types, ret_type) = item in
      (* Find out whether the user-defined types, if any, are valid *)
      if List.for_all (ret_type :: arg_types) ~f:(function
        | Ast.Array(Ast.Type(name)) | Ast.Type(name) ->
          List.exists env_types ~f:(fun (TDefault(type_name, _)) -> type_name = name)
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
  (* make sure fundefs are unique *)
  let nfundefs = check_unique_functions fundefs externs in
  (* temporary env for evaluating tdefaults *)
  let temp_env = {
    scope = { variables=[]; parent=Some(globals) };
    functions = nfundefs;
    extern_functions = externs;
    types = [];
  } in
  let tfuns_ref = ref [] in
  let tdefaults = typed_typedefs temp_env tfuns_ref typedefs in
  let externs = typed_externs externs tdefaults in
  let env = {
    scope = { variables=[]; parent=Some(globals) };
    functions = nfundefs;
    extern_functions = externs;
    types = tdefaults;
  } in
  let sexpr = sast_expr env tfuns_ref (Ast.Block(exprs)) in
  let cpp_includes = List.dedup (List.map externs ~f:(fun (ExternFunDecl(header, _, _, _, _, _)) -> header)) in
  cpp_includes, !tfuns_ref, [sexpr], env.types
