
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
    | Sast.LitStr(x) -> Cast.LitStr(x)
    | Sast.LitUnit -> Cast.LitUnit

    | Sast.Binop(lexpr, op, rexpr)
        -> ignore lexpr; ignore op; ignore rexpr; failwith "Binop cast_sast not implemented"

    | Sast.Uniop(op, expr)
        -> ignore op; ignore expr; failwith "Uniop cast_sast not implemented"

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

    | Sast.Conditional(bexpr,texpr,fexpr)
        -> ignore bexpr; ignore texpr; ignore fexpr; failwith "Conditional cast_sast not implemented"

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
  
let cast_signatures types fundefs =
  (List.map types ~f:(fun (Sast.TDefault(n,_)) -> Cast.SigStruct(n))) @
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
  let signatures = cast_signatures types fundefs in
  let globals = [] in
  let main_expr = (Sast.Block(texprs @ [(Sast.LitInt(0),Ast.Int)]),Ast.Int) in
  cast_incls, signatures, globals, cast_types, castfun_of_sastfun (Sast.FunDef("main",[],main_expr))::cast_fundefs

  