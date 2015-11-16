
open Core.Std
open Ast

type program = Ast.fundef list * Ast.expr list * Ast.typedef list

let get_inchan = function
  | None -> In_channel.stdin
  | Some filename -> In_channel.create (filename^".nh")

let rec noinclu incls_ref (incls, fdefs, externs, exprs, tdefs) =
  List.fold_left (List.rev incls) ~init:(fdefs,externs,exprs,tdefs,incls_ref)
    ~f:(fun (fdefs,externs,exprs,tdefs,incls_ref) next_incl ->
      let (next_fdefs,next_externs,next_exprs,next_tdefs,incls_ref) = 
        (* for each file, ast recursively *)
        if List.mem !incls_ref next_incl
          then
            (fdefs,externs,exprs,tdefs,incls_ref)
          else
            let inchan = get_inchan next_incl in
            let lexbuf = Lexing.from_channel inchan in
            let (incls,fdefs,externs,exprs,tdefs) = Parser.program Scanner.token lexbuf in
            let next_ast = (List.map incls ~f:(fun s -> Some(s)),fdefs,externs,exprs,tdefs) in
            begin
              incls_ref := next_incl :: !incls_ref;
              noinclu incls_ref next_ast
            end
      in
      (next_fdefs@fdefs, next_externs@externs, next_exprs@exprs, next_tdefs@tdefs, incls_ref)
    )

let noinclu_ast name =
  (* keep track of already included files *)
  let incls_ref = ref [] in
  let (new_fdefs, new_externs, new_exprs, new_tdefs, _) = noinclu incls_ref ([Some("../lib/std"); name],[],[],[],[]) in
  (new_fdefs, new_externs, new_exprs, new_tdefs)
