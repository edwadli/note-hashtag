open Core.Std

type program = Ast.fundef list * Ast.externfun list * Ast.expr list * Ast.typedef list

let ast_of_inchan inchan = Parser.program Scanner.token (Lexing.from_channel inchan)

let rec noinclu incls_ref pwd (incls, fdefs, externs, exprs, tdefs) =
  (* The user just types "include myfile" so we need to prepend the search directory and append the extension *)
  let incls = List.map incls ~f:(fun incl -> Filename.concat pwd (incl ^ ".nh")) in
  (* Grab ASTs for all the includes *)
  let incl_fdefs, incl_externs, incl_exprs, incl_tdefs = List.fold_left incls ~init:([], [], [], [])
    ~f:(fun (fdefs, externs, exprs, tdefs) next_incl ->
      let next_fdefs, next_externs, next_exprs, next_tdefs =
        (* for each file, ast recursively *)
        if List.mem !incls_ref next_incl then
          [], [], [], []
        else
          let inchan = In_channel.create next_incl in
          let next_ast = ast_of_inchan inchan in
          begin
            let realpath = Filename.realpath next_incl in
            incls_ref := realpath :: !incls_ref;
            noinclu incls_ref (Filename.dirname realpath) next_ast
          end
      in
      (fdefs @ next_fdefs, externs @ next_externs, exprs @ next_exprs, tdefs @ next_tdefs)
    )
  in
  (* Prepend the included ASTs to our AST *)
  incl_fdefs @ fdefs, incl_externs @ externs, incl_exprs @ exprs, incl_tdefs @ tdefs

let ast_of_filename name =
  (* toplevel ast *)
  let get_inchan = function
    | None -> In_channel.stdin
    | Some(filename) -> In_channel.create filename
  in
  let (incls, fdefs, externs, exprs, tdefs) = ast_of_inchan (get_inchan name) in
  try
    (* keep track of already included files *)
    let seen_incls = ref (match name with None -> [] | Some(x) -> [ Filename.realpath x ]) in
    let pwd = match name with None -> "./" | Some(x) -> Filename.dirname x in
    noinclu seen_incls pwd ("../lib/std" :: incls, fdefs, externs, exprs, tdefs)
  with Unix.Unix_error(error, _, fname) -> failwith (sprintf "%s: %s" fname (Unix.error_message error))
