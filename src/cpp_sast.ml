open Core.Std
open Ast
open Sast
open Cast


let rec castx_of_sastx texpr =
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
        -> ignore varname; ignore expr; failwith "ArrIdx cast_sast not implemented"

    | Sast.Arr(exprs, ast_t)
        -> ignore exprs; ignore ast_t; failwith "Arr cast_sast not implemented"

    | Sast.Block(exprs) ->
        begin match List.rev exprs with
          | [] -> failwith "Internal Error: Sast.Block found to be empty when converting to cast"
          | ret_expr :: body -> Cast.Call(Cast.LambdaRefCap([], t, List.rev begin
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

    | Sast.For(varname, rexpr, dexpr)
        -> ignore varname; ignore rexpr; ignore dexpr; failwith "For cast_sast not implemented"

    | Sast.Throw(lexpr,rexpr)
        -> ignore lexpr; ignore rexpr; failwith "Throw cast_sast not implemented"

    | Sast.Init(name, expr) ->
        let (_,t) = expr in Cast.DeclAssign((t, name), castx_of_sastx expr)

    | Sast.Assign(varname, expr) -> Cast.Assign(varname, castx_of_sastx expr)

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
    body = [ Cast.Return (castx_of_sastx texpr) ];
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

let cast_of_sast (incls, fundefs, texprs, types) =
  let cast_incls = cast_inclus incls in
  let cast_fundefs = List.map fundefs ~f:(castfun_of_sastfun) in
  let cast_types = List.map types ~f:(casttype_of_sasttype) in
  let signatures = cast_signatures fundefs in
  let globals = [] in
  let main_expr = (Sast.Block(texprs @ [(Sast.LitInt(0),Ast.Int)]),Ast.Int) in
  cast_incls, signatures, globals, cast_types, castfun_of_sastfun (Sast.FunDef("main",[],main_expr))::cast_fundefs
