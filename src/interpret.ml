open Core.Std

open Sast

(* WIP -- created because peephole optimizations in optimize.ml had a lot of overlapping code w/ an interpreter *)

let (* rec *) interpreted_of_sast sexpr =
  let (expr, _) = sexpr in
  match expr with
  | Binop((LitBool(v1), t1), op, (LitBool(v2), _)) ->
      (match op with
      | Ast.Eq  -> Sast.LitBool(v1 = v2),  Ast.Bool
      | Ast.Neq -> Sast.LitBool(v1 <> v2), Ast.Bool
      | Ast.Lt  -> Sast.LitBool(v1 < v2),  Ast.Bool
      | Ast.Lte -> Sast.LitBool(v1 <= v2), Ast.Bool
      | _ -> 
          failwith (sprintf "Internal error: can't use %s on type %s" (Ast.string_of_op op) (Ast.string_of_type t1))
      )
  
  | Binop((LitInt(v1), t1), op, (LitInt(v2), _)) ->
      (match op with
      | Ast.Add -> Sast.LitInt (v1 + v2),  Ast.Int
      | Ast.Sub -> Sast.LitInt (v1 - v2),  Ast.Int
      | Ast.Mul -> Sast.LitInt (v1 * v2),  Ast.Int
      | Ast.Div -> Sast.LitInt (v1 / v2),  Ast.Int
      | Ast.Mod -> Sast.LitInt (v1 % v2),  Ast.Int
      | Ast.Eq  -> Sast.LitBool(v1 = v2),  Ast.Bool
      | Ast.Neq -> Sast.LitBool(v1 <> v2), Ast.Bool
      | Ast.Lt  -> Sast.LitBool(v1 < v2),  Ast.Bool
      | Ast.Lte -> Sast.LitBool(v1 <= v2), Ast.Bool
      | _ -> 
          failwith (sprintf "Internal error: can't use %s on type %s" (Ast.string_of_op op) (Ast.string_of_type t1))
      )
  
  | Binop((LitFloat(v1), t1), op, (LitFloat(v2), _)) ->
      (match op with
      | Ast.Add -> Sast.LitFloat(v1 +. v2), Ast.Float
      | Ast.Sub -> Sast.LitFloat(v1 -. v2), Ast.Float
      | Ast.Mul -> Sast.LitFloat(v1 *. v2), Ast.Float
      | Ast.Div -> Sast.LitFloat(v1 /. v2), Ast.Float
      | Ast.Eq  -> Sast.LitBool (v1 = v2),  Ast.Bool
      | Ast.Neq -> Sast.LitBool (v1 <> v2), Ast.Bool
      | Ast.Lt  -> Sast.LitBool (v1 < v2),  Ast.Bool
      | Ast.Lte -> Sast.LitBool (v1 <= v2), Ast.Bool
      | _ -> 
          failwith (sprintf "Internal error: can't use %s. on type %s" (Ast.string_of_op op) (Ast.string_of_type t1))
      )
  
  | Binop((LitStr(v1), t1), op, (LitStr(v2), _)) ->
      (match op with
      | Ast.Concat -> Sast.LitStr (v1 ^ v2),  Ast.String
      | Ast.Eq     -> Sast.LitBool(v1 = v2),  Ast.Bool
      | Ast.Neq    -> Sast.LitBool(v1 <> v2), Ast.Bool
      | _ -> 
          failwith (sprintf "Internal error: can't use %s. on type %s" (Ast.string_of_op op) (Ast.string_of_type t1))
      )
  
  | _ -> failwith "The interpreter doesn't support this kind of expression"
