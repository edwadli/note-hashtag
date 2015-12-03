
open Core.Std
open Sast

exception Cant_infer_type of string

(* environment *)
type symbol_table = {
  parent: symbol_table option;
  variables: (string * Ast.t * Ast.mutability) list;
}

type environment = {
  scope: symbol_table; (* vars symbol table *)
  functions: (int * Ast.fundef) list; (* (num args * fundef) list *)
  extern_functions: Ast.externfun list;
  types: Sast.tdefault list;
}

let rec get_top_scope scope = match scope.parent with
  | None -> scope
  | Some(scope) -> get_top_scope scope

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



let replace_add_fun l item =
  let (Sast.FunDef(name, tparams, (_, _))) = item in
  l := item :: List.filter !l
    ~f:(fun (Sast.FunDef(n, tps, (_, _))) -> name <> n || tparams <> tps)

let check_list_type same_type_list first_t_val =
  (* check if all values in list have same type *)
  let same_type = List.for_all same_type_list ~f:(fun x -> x = first_t_val) in
  if same_type then
    ()
  else
    failwith("Types in Arr don't match t_option value")

let return_list_type exprt =
  let same_type_list = List.map exprt ~f:(fun(_, t) -> t) in
  let first_t_val = List.hd_exn same_type_list in
  check_list_type same_type_list first_t_val;
  first_t_val (* to return the first type *)

let verify_list exprt = 
  if (List.length exprt = 0) then
    failwith("Empty list does not have type")
  else begin
    let first_t_val = return_list_type exprt in (match first_t_val with
    |Ast.Type("pitch") -> Sast.Arr(exprt, Ast.Type("pitch")), Ast.Type("chord")
    |_ -> Sast.Arr(exprt, first_t_val), Ast.Array(first_t_val) ) 
  end

let verify_list_empty orig_t_val exprt = 
  match exprt with
  | [] -> Sast.Arr(exprt, orig_t_val), Ast.Array(orig_t_val)
  | _ ->
    let first_t_val = return_list_type exprt in
    if(orig_t_val = first_t_val) then
      Sast.Arr(exprt, orig_t_val), Ast.Array(orig_t_val)
    else begin
      if orig_t_val = Ast.Type("pitch") then
        Sast.Arr(exprt, Ast.Type("pitch")), Ast.Type("chord")
      else
        failwith("Types in Arr don't match t_option value")
      end

let rec find_variable (scope: symbol_table) name =
  match List.find scope.variables ~f:(fun (s, _, _) -> s = name) with
  | None -> begin
    match scope.parent with
      | Some(parent) -> find_variable parent name
      | None -> raise Not_found
    end
  | Some(x) -> x

let find_ref_type env name fields =
  (* Find the variable called name *)
  let (_, t, _) = find_variable env.scope name in
  (* Call find_field if we're accessing a field of a user-defined type *)
  if fields <> [] then (let (_, (_, t)) = find_field env.types t fields in t) else t

let find_seen_function functions name tparams =
  List.find functions ~f:(fun (n,tps) -> name=n && tps=tparams)

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

let rec convert_music_list exprt typ = (match typ with
  |Ast.Float ->  let same_type_list = List.map exprt ~f:(fun(_, t) -> t) in
                 ignore(check_list_type same_type_list typ);
                 exprt
  |_ -> (match exprt with
    | [] -> []
    | (expr, t) :: rest -> try (chord_of (expr, t)) :: (convert_music_list rest typ)
        with Failure(_) -> failwith("ArrMusic expects type of pitch, int or chord") 
    )
 )

let rec sast_expr ?(seen_funs = []) ?(force = false) env tfuns_ref e =
  let sast_expr_env = sast_expr ~seen_funs:seen_funs env tfuns_ref in
  match e with
  | Ast.LitBool(x) -> Sast.LitBool(x), Ast.Bool
  | Ast.LitInt(x) -> Sast.LitInt(x), Ast.Int
  | Ast.LitFloat(x) -> Sast.LitFloat(x), Ast.Float
  | Ast.LitStr(x) -> Sast.LitStr(x), Ast.String
  | Ast.Binop(lexpr, op, rexpr) ->
    let lexprt = sast_expr_env lexpr in
    let rexprt = sast_expr_env rexpr in
    let (_, lt) = lexprt in
    let (_, rt) = rexprt in
    let opfailwith constraint_str =
      failwith (sprintf "Operator %s is only defined for %s (%s, %s found)"
        (Ast.string_of_op op) constraint_str (Ast.string_of_type lt) (Ast.string_of_type rt))
    in
    begin match op with
      | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div ->
          if lt = rt && (lt = Ast.Float || lt = Ast.Int) then Sast.Binop(lexprt, op, rexprt), lt
          else opfailwith "float or int"
      
      | Ast.Mod ->
          if lt = rt && lt = Ast.Int then Sast.Binop(lexprt,op,rexprt), lt
          else opfailwith "int"

      | Ast.Eq | Ast.Neq ->
          if lt = rt then Sast.Binop(lexprt,op,rexprt), Ast.Bool
          else opfailwith "operands of the same type"
      
      | Ast.Lt | Ast.Lte ->
          if lt = rt && (lt = Ast.Float || lt = Ast.Int) then Sast.Binop(lexprt, op, rexprt), Ast.Bool
          else opfailwith "float or int"
      
      | Ast.And | Ast.Or ->
          if lt = rt && lt = Ast.Bool then Sast.Binop(lexprt,op,rexprt), Ast.Bool
          else opfailwith "bool"

      | Ast.Concat -> begin match lt, rt with
        (* also allow tracks to be concatted *)
        | Ast.Type("track"), Ast.Type("track") -> Sast.FunApply(NhFunction("ConcatTracks"),[lexprt;rexprt]), lt
        | Ast.Array(l), Ast.Array(r) when l = r -> Sast.Binop(lexprt,op,rexprt), lt
        | _ -> failwith "Concat is only for defined for same typed arrays and tracks" end

      | Ast.Chord ->
          (* guarantee that chord binop is between two chords *)
          Sast.FunApply(NhFunction("ChordOfChords"),[chord_of lexprt; chord_of rexprt]), Ast.Type("chord")

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
    let exprt = sast_expr_env expr in
    let (_, t) = exprt in
    begin match op with
      | Ast.Not when t = Ast.Bool -> Sast.Uniop(op, exprt), t
      | Ast.Not -> failwith "This operator is only defined for bool"
      | Ast.Neg when t = Ast.Int || t = Ast.Float -> Sast.Uniop(op, exprt), t
      | Ast.Neg -> failwith "This operator is only defined for int or float"
      
      | Ast.Sharp | Ast.Flat when t = Ast.Int || t = Ast.Type("pitch")
        -> Sast.Uniop(op, exprt), Ast.Type("pitch")
      | Ast.Sharp -> let tpitch = Ast.Type("pitch") in
          let exprt = match t with
            | Ast.Int -> Sast.FunApply(NhFunction("PitchOfInt"), [exprt]), tpitch
            | Ast.Type("pitch") -> exprt
            | _ -> failwith "sharp is only defined for int or pitch"
          in Sast.FunApply(NhFunction("SharpPitch"), [exprt]), tpitch
      | Ast.Flat -> let tpitch = Ast.Type("pitch") in
          let exprt = match t with
            | Ast.Int -> Sast.FunApply(NhFunction("PitchOfInt"), [exprt]), tpitch
            | Ast.Type("pitch") -> exprt
            | _ -> failwith "flat is only defined for int or pitch"
          in Sast.FunApply(NhFunction("FlatPitch"), [exprt]), tpitch
    end
  
  | Ast.FunApply(name, arg_exprs) ->
    (* Common code for all function calls *)
    (* get typed versions of input expressions *)
    let arg_texprs = List.map arg_exprs ~f:(sast_expr_env) in
    (* get types of the typed arguments *)
    let arg_types = List.map arg_texprs ~f:(fun (_, t) -> t) in
    
    (* NH function calls *)
    if is_nh_function env name
    then
      (* find the function *)
      let num_args = List.length arg_exprs in
      let nh_fun_sig = find_function env.functions name num_args in
      let (_,Ast.FunDef(_,params,expr)) = nh_fun_sig in
      (* zip params with input types *)
      let tparams = match List.zip params arg_types with
        | None -> failwith "Internal error: Mismatched lengths of types and arguments while type checking function call"
        | Some(x) -> x
      in
      let has_seen_fun = (find_seen_function seen_funs name tparams) <> None in
      (* check if function type already inferred *)
      match List.find !tfuns_ref ~f:(fun (Sast.FunDef(n,tps,_)) -> name=n && tparams=tps) with
        (* already know function signature and not forced to re-infer subexpression types
          (or is forced but loop encountered, so need to use previous result anyway) *)
        | Some(Sast.FunDef(_,_,(_,t))) when (force && has_seen_fun) || not force ->
            Sast.FunApply(NhFunction(name), arg_texprs), t
        (* don't know function signature, or do know signature but
          forced to re-infer subexpression types *)
        | _ ->
          let seen_funs =
            if has_seen_fun
            (* if the function is already seen, we are in a loop - can't infer type;
              roll back to most recent conditional and see what we can do there
              (conditional catches Cant_infer_type exception) *)
            then raise
              (Cant_infer_type("Can't infer type of recursive call to nh function "^name))
            (* function not seen yet; we need to infer subexpression types, so mark this function as seen *)
            else (name,tparams):: seen_funs
          in
          let try_check_function_type force =
            (* forcing type inference means that cached results in
              tfuns_ref will be ignored unless a loop is encountered *)
            try check_function_type tparams expr tfuns_ref env seen_funs force
            with Failure(reason) ->
              Log.info "Function template type check failed. Inner exception: %s" reason;
              failwith ("Incorrect types passed into function "^name)
          in
          (* check if types of inputs can be used with this function *)
          let (sexpr, t) = try_check_function_type force in
          begin
          (* UPDATE tfuns_ref *)
          ignore(replace_add_fun tfuns_ref (Sast.FunDef(name, tparams, (sexpr, t))));
          (* check if it is safe to re-infer all subexpression types (ie tfun_ref is fully updated)
              and that we are not already trying to re-infer all subexpression types *)
          if List.length seen_funs = 1 && not force
            (* go back in to resolve all types; 
              second pass guarantees no placeholder conditionals are in descendants *)
            then let (sexpr, t) = try_check_function_type true in
              ignore(replace_add_fun tfuns_ref (Sast.FunDef(name, tparams, (sexpr, t))));
              Sast.FunApply(NhFunction(name), arg_texprs), t
            (* pass sast back up the ast so higher expressions can infer type;
              could still have placeholder conditionals in descendants *)
            else Sast.FunApply(NhFunction(name), arg_texprs), t
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
                | Sast.Init(name, (_, t), mutability), _ ->
                    let new_vars = (name, t, mutability) :: env.scope.variables in
                    let new_scope = { parent=env.scope.parent; variables=new_vars } in
                    { scope=new_scope; functions=env.functions;
                      extern_functions=env.extern_functions; types=env.types; }
                | _ -> env
              end
        in let texpr = sast_expr ~seen_funs:seen_funs env tfuns_ref expr in
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
  
  | Conditional(condition, case_true, case_false) ->
      let (condition, condition_t) = sast_expr_env condition in
      if condition_t <> Ast.Bool then
        failwith (sprintf "Condition must be a bool expression (%s found)" (Ast.string_of_type condition_t)) else

      let try_sast_expr_env expr =
        try Some(sast_expr_env expr)
        (* Note that Cant_infer_type is raised when an nh function has already
          been seen higher up in the ast and it's not in tfuns_ref either *)
        (* Also note that this case won't trigger if force was set to true (from FunApply) *)
        with Cant_infer_type(_) -> None
      in
      (* try to infer type of true branch *)
      begin match try_sast_expr_env case_true with
        (* true branch type inference failed,
          ie tfuns_ref is missing an nh function that has been used higher up in the ast *)
        | None ->
            (* see if other case can infer type *)
            begin match try_sast_expr_env case_false with
              (* neither branch terminates *)
              | None -> failwith "Couldn't infer type of either branch of conditional"
              (* only false branch terminates, assume entire conditional is of that type;
                return fake sast with correct type so that tfuns_ref can be updated *)
              | Some((_,t)) ->
                  let fake_sexpr = (Sast.LitUnit, t) in
                  Sast.Conditional((condition, condition_t), fake_sexpr, fake_sexpr), t
            end
        (* true branch type inference successful, now check false branch *)
        | Some((case_true, case_true_t)) ->
            (* see if other case can infer type *)
            begin match try_sast_expr_env case_false with
              (* both branch types have been inferred, check if types are the same *)
              | Some((case_false, case_false_t)) -> if case_true_t <> case_false_t
                  then failwith (sprintf "Both expressions in a conditional must have the same type (%s and %s found)"
                    (Ast.string_of_type case_true_t) (Ast.string_of_type case_false_t))
                  else Sast.Conditional( (condition, condition_t), (case_true, case_true_t), (case_false, case_false_t) ), case_true_t
              (* false branch type inference failed, ie tfuns_ref is missing an nh function that
                  has been used higher up in the ast;
                true branch terminates, assume entire conditional is of that type;
                return fake sast with correct type so that tfuns_ref can be updated *)
              | None -> let fake_sexpr = (Sast.LitUnit, case_true_t) in
                  Sast.Conditional((condition, condition_t), fake_sexpr, fake_sexpr), case_true_t
            end
      end
  
  | For(loop_var_name, items, body) ->
    ignore (loop_var_name, items, body); failwith "Type checking not implemented for For"
  
  | Throw(msg_expr) -> let (_,t) = sast_expr_env msg_expr in
      if t <> Ast.String then failwith "throw expects an expression of type string"
      else let msg = sast_expr_env (Ast.FunApply("PrintEndline",[msg_expr])) in
      Sast.Block([msg; (Sast.Exit(0), Ast.Unit)]), Ast.Unit
  
  | Ast.Assign(names, expr, mutability) ->
      (* Type-check RHS of the assignment *)
      let (value, tvalue) = sast_expr_env expr in
      begin
        match names with
        (* No variable name *)
        | [] -> failwith "Internal error: Assign(names, _, _) had empty string list"
        | name :: fields ->
            try
              (* Check that variable is mutable *)
              let (_, _, mutability) = find_variable env.scope name in
              if mutability = Immutable then failwith (sprintf "cannot assign to immutable %s" name) else
              (* Check that types match *)
              let t = find_ref_type env name fields in
              if t <> tvalue then failwith (sprintf "cannot assign type %s to %s (type %s)"
                (Ast.string_of_type tvalue) (Ast.string_of_expr (VarRef(names))) (Ast.string_of_type t)) else
              (* Passed all checks! *)
              Sast.Assign(names, (value, tvalue)), Ast.Unit
            with Not_found ->
              match fields with
              | [] -> Init(name, (value, tvalue), mutability), Ast.Unit
              | _ -> failwith ("Cannot assign to fields of uninitialized var " ^ (Ast.string_of_expr (VarRef(names))))
      end
  
  | Ast.StructInit(typename, init_list) ->
      let TDefault(_, defaults) = match List.find env.types ~f:(fun (TDefault(n,_)) -> typename = n) with
        | Some(x) -> x
        | None -> failwith ("type "^typename^" not found")
      in
      let fields = List.map defaults ~f:(fun (n,(_,t)) -> (n,t)) in
      let sexprs = List.map init_list ~f:(sast_expr_env) in
      let varname = function
        | Sast.Init(name, (_, t), _), _
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
            | Init(_, expr, _), _ -> expr
            | Assign(_, expr), _ -> expr
            | _ -> failwith ("Internal error: non init/assign sexpr found in type init")
          in
          match List.find sexprs ~f:(fun sexpr -> name = varname sexpr) with
            | Some(x) -> (name, field_expr x) :: init_exprs
            | None -> (name, expr) :: init_exprs
        )
      in
      Struct(typename, init_exprs), Ast.Type(typename)

  | Ast.Arr(expr_list, t_op) ->
      (* check if t_op is None or Sum *)
      begin match t_op with
      | None -> 
        let exprt = List.map expr_list ~f:(sast_expr_env) in
        verify_list exprt
      (* t_op actually exists *)
      | Some v ->
        (* Assign t_val to t_op value in Arr *)
        let orig_t_val = v in
        let exprt = List.map expr_list ~f:(sast_expr_env) in
        verify_list_empty orig_t_val exprt
      end

  | Ast.ArrMusic(expr_list, t_op) ->
      begin match t_op with
      | None ->
          let exprt = List.map expr_list ~f:(sast_expr_env) in
          if (List.length exprt = 0) then
            failwith("Empty list does not have type")
          else 
            let (_, first_t_val) = List.hd_exn exprt in
            let music_list = convert_music_list exprt first_t_val in
            verify_list music_list
      | Some v ->
        let orig_t_val =
          if v <> Ast.Float then
            Ast.Type("chord")
          else
            Ast.Float
        in
        let exprt = List.map expr_list ~f:(sast_expr_env) in
        let music_list = convert_music_list exprt orig_t_val in
        verify_list_empty orig_t_val music_list
    end

  |Ast.ArrIdx(id_var, expr) ->
    let (exp, t) = sast_expr env tfuns_ref expr in
      if t <> Ast.Int then
        failwith(sprintf "Array Index must be an integer (%s found)" (Ast.string_of_type t))
      else
        let (_, t_v) = sast_expr env tfuns_ref (Ast.VarRef(id_var)) in
        match t_v with
        |Ast.Array(x) -> Sast.ArrIdx(id_var, (exp, t)), x
        |_ -> failwith("Cannot index into a non-array object")

and check_function_type tparams expr tfuns_ref env seen_funs force = 
  let tparams' = List.map tparams ~f:(fun (name, t) -> (name, t, Ast.Mutable)) in
  let env' = {
    (* allow global scope *)
    scope = { variables = tparams'; parent = Some(get_top_scope env.scope) };
    functions = env.functions;
    extern_functions = env.extern_functions;
    types = env.types;
  } in
  sast_expr ~seen_funs:seen_funs ~force:force env' tfuns_ref expr

and typed_typedefs env tfuns_ref typedefs =
  (* Assuming the users have defined the types in the correct order *)
  let (tdefaults, _) = List.fold_left typedefs ~init:([],env)
    ~f:(fun (tdefaults, env) (Ast.TypeDef(name, exprs)) ->
      let sexprs = List.map exprs ~f:(sast_expr env tfuns_ref) in
      let fields = List.map sexprs
        ~f:(fun sexpr ->
          match sexpr with
            | Init(name, expr, _), _ -> (name, expr)
            | Assign(name::tail, expr),_ when List.length tail = 0 -> (name, expr)
            | _ -> failwith ("Only initialization of fields are allowed in type decl of "^name)
        )
      in
      if List.contains_dup fields
        ~compare:(fun (ln,_) (rn,_) -> compare ln rn)
        then failwith ("Cannot init fields multiple times in type decl of "^name)
      else
      let tdefaults = TDefault(name, fields)::tdefaults in
      let env = 
        {
          scope = env.scope;
          functions = env.functions;
          extern_functions = env.extern_functions;
          types = tdefaults;
        }
      in tdefaults, env
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

let rec verify_no_fun_ast ast =
  let verify_all exprs =
    List.iter exprs ~f:verify_no_fun_ast
  in match ast with
    | Ast.FunApply(_,_) -> failwith "Function found"
    | LitBool(_) | LitFloat(_) | LitInt(_) | LitStr(_) | VarRef(_) -> ()
    | Binop(lexpr,_,rexpr) | For(_,lexpr,rexpr) -> verify_all [lexpr; rexpr]
    | Uniop(_,expr) | ArrIdx(_,expr) | Throw(expr) | Assign(_,expr,_) -> verify_all [expr]
    | Conditional(bexpr,texpr,fexpr) -> verify_all [bexpr; texpr; fexpr]
    | Arr(exprs) | ArrMusic(exprs) | Block(exprs) | StructInit(_,exprs) -> verify_all exprs

(* Note that includes have been processed and merged into exprs by this point *)
let sast_of_ast (fundefs, externs, exprs, typedefs) =
  (* ensure there are no function calls in typedefs *)
  List.iter typedefs ~f:(fun (Ast.TypeDef(name,exprs)) ->
    try List.iter exprs ~f:verify_no_fun_ast
    with Failure(_) -> failwith ("Function call found in typedef "^name)
  );
  (* make sure fundefs are unique *)
  let nfundefs = check_unique_functions fundefs externs in
  (* temporary env for evaluating tdefaults *)
  let temp_env = {
    scope = { variables=[]; parent=None };
    functions = nfundefs;
    extern_functions = externs;
    types = [];
  } in
  let tfuns_ref = ref [] in
  let tdefaults = typed_typedefs temp_env tfuns_ref typedefs in
  let externs = typed_externs externs tdefaults in
  let env = {
    scope = { variables=[]; parent=None };
    functions = nfundefs;
    extern_functions = externs;
    types = tdefaults;
  } in
  let sexpr = sast_expr env tfuns_ref (Ast.Block(exprs)) in
  let cpp_includes = List.dedup (List.map externs ~f:(fun (ExternFunDecl(header, _, _, _, _, _)) -> header)) in
  cpp_includes, !tfuns_ref, [sexpr], env.types
