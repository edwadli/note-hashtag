open Core.Std
open Ast
open Sast
open Cast


let rec castx_of_sastx texpr =
  let unit_ret = Cast.Return(Cast.LitUnit) in
  let (expr, t) = texpr in
  match expr with
    | Sast.LitBool(x) -> Cast.LitBool(x)
    | Sast.LitInt(x) -> Cast.LitInt(x)
    | Sast.LitFloat(x) -> Cast.LitFloat(x)
    (* Comparison of string literals in C++ is undefined (you are actually comparing the two char* values) *)
    | Sast.LitStr(x) -> Cast.Call(Function("std", "string"), [ Cast.LitStr(x) ])
    | Sast.LitUnit -> Cast.LitUnit

    | Sast.Binop(lexpr, op, rexpr) ->
        begin match op with
          | Ast.Zip -> failwith "Internal error: binop zip should have been converted to Call NhFunction in ast2sast"
          | Ast.Concat ->
              Cast.Call(Cast.Function("nh_support","concat"),[castx_of_sastx lexpr; castx_of_sastx rexpr])
          | Ast.Chord -> failwith "Internal error: binop chord should have been converted to Call NhFunction in ast2sast"
          | Ast.Octave -> failwith "Internal error: binop octave should have been converted to Call NhFunction in ast2sast"
          | _ as cop -> let op = begin match cop with
            | Ast.Add -> Cast.Add
            | Ast.Sub -> Cast.Sub
            | Ast.Mul -> Cast.Mult
            | Ast.Div -> Cast.Div
            | Ast.Mod -> Cast.Mod
            | Ast.Eq -> Cast.Equal
            | Ast.Neq -> Cast.Neq
            | Ast.Lt -> Cast.Less
            | Ast.Lte -> Cast.Leq
            | Ast.And -> Cast.And
            | Ast.Or -> Cast.Or
            | _ -> failwith "Internal error: failed to match all possible binops in sast2cast"
          end in Cast.Binop(castx_of_sastx lexpr, op, castx_of_sastx rexpr)
        end


    | Sast.Uniop(op, expr) ->
        begin match op with
          | Ast.Not -> Cast.Uniop(Cast.Not, castx_of_sastx expr)
          | Ast.Neg -> Cast.Uniop(Cast.Neg, castx_of_sastx expr)
          | _ -> failwith
              "Internal error: uniop flat and sharp should have been converted to Call NhFunction in ast2sast"
        end

    | Sast.VarRef(names) -> Cast.VarRef(names)

    | Sast.FunApply(fname, exprs) ->
        let (ns, fn) = match fname with
          | NhFunction(name) -> "", name
          | CppFunction(_, ns, name) -> ns, name
        in Cast.Call(Cast.Function(ns, fn), List.map exprs ~f:(castx_of_sastx))

    | Sast.ArrIdx(varname, expr)
      -> Cast.Idx(varname, castx_of_sastx expr)

    | Sast.Arr(exprs, ast_t) ->
        let start = match ast_t with
        |Ast.Type(_) -> "struct "
        |_ -> "" in
        let template_type = "vector<" ^ start ^ Cast.string_of_type(ast_t) ^ ">" in
        Cast.Call(Function("std", template_type), [Cast.InitList(List.map exprs ~f:(castx_of_sastx) )])

    | Sast.Block(exprs) ->
        begin match List.rev exprs with
          | [] -> failwith "Internal Error: Sast.Block found to be empty when converting to cast"
          | ret_expr :: body ->
              (* cannot return assignments; pad with lit unit *)
              let (ret_expr,body) = 
                match ret_expr with
                  | Sast.Init(_, _, _),_ -> (Sast.LitUnit, Ast.Unit), ret_expr::body
                  | _ -> ret_expr, body
              in
              Cast.Call(Cast.LambdaRefCap([], t, List.rev begin
                Cast.Return(castx_of_sastx ret_expr) ::
                List.map body ~f:(fun expr -> Cast.Expr(castx_of_sastx expr)) end), [])
        end

    | Sast.Conditional(condition, case_true, case_false) ->
        let condition = castx_of_sastx condition in
        (* Save the type before we throw it away *)
        let (_, ret_t) = case_true in
        (* Throw the expression into a block if it's not already a block *)
        let wrap_nonblock sexpr =
          match sexpr with
          | Sast.Block(_), _ -> sexpr
          | _, t -> Sast.Block([ sexpr ]), t
        in
        let case_true = castx_of_sastx (wrap_nonblock case_true)
        and case_false = castx_of_sastx (wrap_nonblock case_false) in
        (* Create if statement *)
        let if_stmt = Cast.If(condition, Cast.Return(case_true), Cast.Return(case_false)) in
        (* No need for Cast.Block because castx_of_sastx wraps each case in a block in a lambda *)
        Cast.Call(Cast.LambdaRefCap([], ret_t, [ if_stmt ]), [])

    | Sast.For((var_name, var_t), rexpr, dexpr) ->
        let for_stmt = Cast.ForRange((var_t, var_name), castx_of_sastx rexpr, Cast.Expr(castx_of_sastx dexpr)) in
        Cast.Call(Cast.LambdaRefCap([], Ast.Unit, [ for_stmt; unit_ret ]), [])

    | Sast.Exit(code) ->
        let cast_exit = Cast.Call(Cast.Function("","exit"),[Cast.LitInt(code)]) in
        Cast.Call(Cast.LambdaRefCap([], Ast.Unit, [Cast.Expr(cast_exit); unit_ret]), [])

    | Sast.Init(name, expr, _) ->
        let (_,t) = expr in Cast.DeclAssign((t, name), castx_of_sastx expr)

    | Sast.Assign(varname, expr) -> Cast.Call(
        (* assign and then return unit *)
        Cast.LambdaRefCap([], Ast.Unit,
          [Cast.Expr(Cast.Assign(varname, castx_of_sastx expr));
          Cast.Return(Cast.LitUnit)]),[])

    | Sast.Struct(typename, fields) ->
      let fields = List.sort fields ~cmp:(fun (ln,_) (rn,_) -> compare ln rn) in
      let args = List.map fields ~f:(fun (_,expr) -> castx_of_sastx expr) in
      Cast.Call(Cast.Struct(typename), args)

let castfun_of_sastfun fundef =
  let Sast.FunDef(fname, fargs, texpr) = fundef in
  let (_, return_type) = texpr in
  {
    fnamespace = "";
    fname = fname;
    fargs = List.map fargs ~f:(fun (s,t) -> (t,s));
    treturn = return_type;
    body = [ Cast.Return (castx_of_sastx (Sast.Block([texpr]),return_type)) ];
  }

let casttype_of_sasttype (Sast.TDefault(name, fields)) =
  let fields = List.sort fields ~cmp:(fun (ln,_) (rn,_) -> compare ln rn) in
  let sargs = List.map fields ~f:(fun (n, (_,t)) -> (t,n)) in
  {
    sname = name;
    sargs = sargs;
  }
  
let cast_signatures fundefs =
  (List.map fundefs ~f:(fun (Sast.FunDef(n,args,(_,tret))) ->
    let decls = List.map args ~f:(fun (s,t)->(t,s)) in
    Cast.SigFunc(n,tret,decls)))

let cast_inclus incls =
  let defaults = ["iostream"; "string"; "vector"] in
  List.map defaults ~f:(fun s -> Cast.IncludeAngleBrack(s)) @
  List.map incls ~f:(fun s -> Cast.IncludeQuote(s))

let rec verify_no_assign_expr = function
  | Cast.DeclAssign(_,_) -> failwith "Cannot initialize a variable in this context"
  | Cast.Assign(_,_) -> failwith "Cannot assign to a variable in this context"
  | _ as expr -> verify_expr expr

and verify_expr = function
  (* traverse down a level of the ast *)
  | Cast.DeclAssign(_,expr) | Cast.Assign(_,expr) | Cast.Idx(_,expr)
  | Cast.Uniop(_,expr) ->
      verify_no_assign_expr expr
  | Cast.Binop(lexpr,_,rexpr) -> ignore(verify_no_assign_expr lexpr); verify_no_assign_expr rexpr
  | Cast.Call(callable,exprs) -> ignore(List.map exprs ~f:verify_no_assign_expr);
      begin match callable with
        | Cast.LambdaRefCap(_,_,stmts) -> ignore(List.map stmts ~f:verify_no_init_stmt)
        | Cast.Method(_) | Cast.Function(_,_) | Cast.Struct(_) -> ()
      end
  | Cast.LitUnit | Cast.LitBool(_) | Cast.LitInt(_) | Cast.LitFloat(_) | Cast.LitStr(_)
  | Cast.InitList(_) | Cast.Decl(_) | Cast.VarRef(_) | Noexpr -> ()

and verify_no_init_stmt = function
  | Cast.Block(stmts) -> ignore(List.map stmts ~f:verify_no_init_stmt)
  | Cast.Expr(expr) -> ignore(verify_expr expr)
  | Cast.Return(expr) -> ignore(verify_expr expr)
  | Cast.If(expr, lstmt, rstmt) -> 
      ignore(verify_expr expr); ignore(verify_no_init_stmt lstmt);
      ignore(verify_no_init_stmt rstmt)
  | Cast.For(lexpr, mexpr, rexpr, stmt) ->
      ignore(verify_expr lexpr); ignore(verify_expr mexpr);
      ignore(verify_expr rexpr); ignore(verify_no_init_stmt stmt)
  | Cast.While(expr, stmt) -> ignore(verify_expr expr); ignore (verify_no_init_stmt stmt)
  | Cast.ForRange(_, expr, stmt) -> ignore(verify_expr expr); ignore(verify_no_init_stmt stmt)

let rec verify_no_init funs = 
  ignore begin
    match funs with
      | [] -> ()
      | head::tail -> begin ignore(verify_no_init tail);
          ignore(
            let { fnamespace=_;fname=_;fargs=_;treturn=_; body=body } = head in
            List.map body ~f:verify_no_init_stmt
          )
        end
  end; funs

let strip_top_level = function
  | [(Sast.Block(texprs),t)] -> let (texprs,globals) = List.fold_left texprs ~init:([],[])
      (* change all top level inits to assignments *)
      ~f:(fun (texprs, globals) texpr ->
        match texpr with
          | Sast.Init(name,(expr,t),_),tunit ->
            (Sast.Assign([name],(expr,t)),tunit)::texprs, (t,name)::globals
          | _ -> texpr::texprs, globals
      )
      in ([Sast.Block(List.rev texprs),t], globals)
  | [(Sast.LitUnit,Ast.Unit)] as texprs -> texprs,[]
  | _ -> failwith "Internal Error: could not extract globals because top level was not Block"

let cast_of_sast (incls, fundefs, texprs, types) =
  let cast_incls = cast_inclus incls in
  let cast_fundefs = List.map fundefs ~f:(castfun_of_sastfun) in
  let cast_types = List.map types ~f:(casttype_of_sasttype) in
  let ssignatures = List.map cast_types
    ~f:(fun {sname=n; sargs=_} -> Cast.SigStruct(n)) in
  let fsignatures = cast_signatures fundefs in
  let (sexprs, globals) = strip_top_level texprs in
  let main_expr = (Sast.Block(sexprs @ [(Sast.LitInt(0),Ast.Int)]),Ast.Int) in
  let all_funs = castfun_of_sastfun (Sast.FunDef("main",[],main_expr))::cast_fundefs in
  let verified_funs = verify_no_init all_funs in
  cast_incls, ssignatures@fsignatures, globals, cast_types, verified_funs
