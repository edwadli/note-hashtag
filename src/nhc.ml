open Ast

let _ =
  let lexbuf = Lexing.from_string "123" in
  let _ = Parser.block Scanner.token lexbuf in
  print_endline "🎵 #️⃣ c: scanner and parser ran without crashing"
