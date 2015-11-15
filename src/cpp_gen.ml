
open Core.Std
open Sast

let all_includes includes = 
  let defaults = [
    "iostream";
    "string";
    "vector"
    (* "stk/FileLoop.h"; *)
    (* "stk/FileWvOut.h" *)
  ] in
  List.fold_left (defaults @ includes) ~init:"" ~f:(fun l hname -> l^"#include \""^hname^"\"\n" )

let function_prototypes fundefs = ""

let struct_prototypes types = ""

let function_defs fundefs = ""

let struct_defs types = ""

let rec cpp_expr = function
  | LitBool(b) -> if b then "true" else "false"
  | LitInt(i) ->  string_of_int i
  | LitFloat(f) -> Float.to_string f
  | LitStr(s) -> "\""^s^"\""
  | FunApply(fname, exprs) -> fname^"("^
      String.concat ~sep:","
        (List.map exprs ~f:(fun (expr, _) -> "("^cpp_expr expr^")"))
      ^")"
  | _ -> failwith "rest of sast not converted yet"

(* TODO keep track of namespace *)
let cpp_of_sast (includes, fundefs, exprs, types) =
  (* add includes *)
  (* generate function prototypes *)
  (* generate struct prototypes *)
  all_includes includes ^ function_prototypes fundefs ^ struct_prototypes types ^
  (* add function defs and type defs *)
  function_defs fundefs ^ struct_defs types ^
  (* add program exprs *)
  List.fold_left exprs ~init:"" ~f:(fun prog expr -> prog ^ cpp_expr expr ^ ";\n")
