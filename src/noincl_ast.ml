
open Core.Std
open Ast

type program = Ast.fundef list * Ast.expr list * Ast.typedef list

let get_inchan = function
  | None -> In_channel.stdin
  | Some filename -> In_channel.create filename

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

let rec verify_no_fun_ast ast =
  let verify_all exprs =
    List.iter exprs ~f:verify_no_fun_ast
  in match ast with
    | FunApply(_,_) -> failwith "Function found"
    | LitBool(_) | LitFloat(_) | LitInt(_) | LitStr(_) | VarRef(_) -> ()
    | Binop(lexpr,_,rexpr) | For(_,lexpr,rexpr) -> verify_all [lexpr; rexpr]
    | Uniop(_,expr) | ArrIdx(_,expr) | Throw(expr) | Assign(_,expr) -> verify_all [expr]
    | Conditional(bexpr,texpr,fexpr) -> verify_all [bexpr; texpr; fexpr]
    | Arr(exprs) | ArrMusic(exprs) | Block(exprs) | StructInit(_,exprs) -> verify_all exprs

let noinclu_ast name =
  (* keep track of already included files *)
  let incls_ref = ref [] in
  let (new_fdefs, new_externs, new_exprs, new_tdefs, _) = noinclu incls_ref ([Some("../lib/std.nh"); name],[],[],[],[]) in
  (* ensure there are no function calls in typedefs *)
  List.iter new_tdefs ~f:(fun (TypeDef(name,exprs)) ->
    try List.iter exprs ~f:verify_no_fun_ast
    with Failure(_) -> failwith ("Function call found in typedef "^name)
  );
  (new_fdefs, new_externs, new_exprs, new_tdefs)
