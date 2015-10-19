open Ast

let _ =
  let lexbuf = Lexing.from_string "123" in
  let expr = Parser.expr Scanner.token lexbuf in
  print_endline "🎵 #️⃣ c: scanner and parser ran without crashing"
