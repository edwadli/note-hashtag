
open Core.Std
open Ast

type program = Ast.fundef list * Ast.expr list * Ast.typedef list


let rec noinclu incls_ref (incls, fdefs, externs, exprs, tdefs) =
  List.fold_left (List.rev incls) ~init:(fdefs,externs,exprs,tdefs,incls_ref)
    ~f:(fun (fdefs,externs,exprs,tdefs,incls_ref) next_incl ->
      let (next_fdefs,next_externs,next_exprs,next_tdefs,incls_ref) = 
        (* for each file, ast recursively *)
        if List.mem !incls_ref next_incl
          then
            (fdefs,externs,exprs,tdefs,incls_ref)
          else
            let inchan = In_channel.create (next_incl^".nh") in
            let lexbuf = Lexing.from_channel inchan in
            let next_ast = Parser.program Scanner.token lexbuf in begin
            incls_ref := next_incl :: !incls_ref;
            noinclu incls_ref next_ast end
      in
      (next_fdefs@fdefs, next_externs@externs, next_exprs@exprs, next_tdefs@tdefs, incls_ref)
    )

let noinclu_ast name =
  (* keep track of already included files *)
  let incls_ref = ref [] in
  let (new_fdefs, new_externs, new_exprs, new_tdefs, _) = noinclu incls_ref (["std"; name],[],[],[],[]) in
  (new_fdefs, new_externs, new_exprs, new_tdefs)
