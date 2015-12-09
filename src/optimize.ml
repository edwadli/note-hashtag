open Core.Std

open Sast

let rec constfold sexpr =
  let (expr, exprt) = sexpr in
  match expr with
  | Binop(lexpr, op, rexpr) ->
      let lexpr = constfold lexpr and rexpr = constfold rexpr in
      let (v1, _) = lexpr and (v2, _) = rexpr in
      let foldable =
        match v1, v2 with
        | LitInt(v1), LitInt(v2) -> Log.debug "constfold found: %d %s %d" v1 (Ast.string_of_op op) v2; true
        | LitFloat(v1), LitFloat(v2) -> Log.debug "constfold found: %f %s. %f" v1 (Ast.string_of_op op) v2; true
        | LitStr(v1), LitStr(v2) -> Log.debug "constfold found: \"%s\" %s \"%s\"" v1 (Ast.string_of_op op) v2; true
        | _ -> false
      in
      if foldable then Interpret.interpreted_of_sast (Binop(lexpr, op, rexpr), exprt)
      else Binop(lexpr, op, rexpr), exprt
  
  (* Now we get to write out all the recursive cases...there's gotta be a better way of doing this *)
  
  | Uniop(op, expr) ->
      let expr = constfold expr in
      Uniop(op, expr), exprt
  
  | FunApply(fname, exprs) ->
      let exprs = List.map exprs ~f:constfold in
      FunApply(fname, exprs), exprt
  
  | ArrIdx(varref, expr) ->
      let expr = constfold expr in
      ArrIdx(varref, expr), exprt
  
  | Arr(exprs, t) ->
      let exprs = List.map exprs ~f:constfold in
      Arr(exprs, t), exprt
  
  | Block(exprs) ->
      let exprs = List.map exprs ~f:constfold in
      Block(exprs), exprt
  
  | Conditional(e1, e2, e3) ->
      let e1 = constfold e1
      and e2 = constfold e2
      and e3 = constfold e3 in
      Conditional(e1, e2, e3), exprt
  
  | For(vdecl, e1, e2) ->
      let e1 = constfold e1
      and e2 = constfold e2 in
      For(vdecl, e1, e2), exprt
  
  | Init(name, expr, mutability) ->
      let expr = constfold expr in
      Init(name, expr, mutability), exprt
  
  | Assign(name, expr) ->
      let expr = constfold expr in
      Assign(name, expr), exprt
  
  | Struct(name, defaults) ->
      let defaults = List.map defaults ~f:(fun (name, expr) -> (name, constfold expr)) in
      Struct(name, defaults), exprt
  
  (* If we don't have children to check or an optimization to apply, just return the same thing *)
  | _ -> sexpr
  
