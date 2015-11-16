
open Core.Std
open Sast

let all_includes includes = 
  let defaults = [
    "<iostream>";
    "<string>";
    "<vector>"
    (* "stk/FileLoop.h"; *)
    (* "stk/FileWvOut.h" *)
  ] in
  List.fold_left (defaults @ List.map (List.dedup includes) ~f:(fun s -> "\""^s^"\""))
    ~init:"" ~f:(fun l hname -> l^"#include "^hname^"\n" )

let hpp_of_fundefs fundefs = let _ = fundefs in ""

let hpp_of_typedefs types = let _ = types in ""

let cpp_of_fundefs fundefs = let _ = fundefs in ""

let cpp_of_typedefs types = let _ = types in ""

let rec cpp_of_expr texpr =
  let (expr, _) = texpr in match expr with
  | LitBool(b) -> if b then "true" else "false"
  | LitInt(i) ->  string_of_int i
  | LitFloat(f) -> sprintf "%.17G" f
  | LitStr(s) -> "\""^String.escaped s^"\""
  | FunApply(fname, exprs) -> let fun_name = match fname with
      | NhFunction(fn) -> fn
      | CppFunction(_,ns,fn) -> ns^"::"^fn
    in
    fun_name^"("^
      String.concat ~sep:","
        (List.map exprs ~f:(fun texpr -> "("^cpp_of_expr texpr^")"))
      ^")"
  | _ -> failwith "rest of sast not converted yet"

(* TODO keep track of namespace *)
let cpp_of_sast (includes, fundefs, exprs, types) =
  (* add includes *)
  (* generate function prototypes *)
  (* generate struct prototypes *)
  all_includes includes ^ hpp_of_typedefs types ^ hpp_of_fundefs fundefs ^
  (* add function defs and type defs *)
  cpp_of_typedefs types ^ cpp_of_fundefs fundefs ^
  (* add program exprs *)
  "int main() {\n"^
  List.fold_left exprs ~init:"" ~f:(fun prog expr -> prog ^ cpp_of_expr expr ^ ";\n")^
  "}\n"
