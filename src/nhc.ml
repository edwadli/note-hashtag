open Ast
open Log
open Test

let _ =
  let lexbuf = Lexing.from_string "123\n" in
  let _ = Parser.program Scanner.token lexbuf in
  Log.info "scanner and parser ran without crashing";
