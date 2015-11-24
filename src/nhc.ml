open Core.Std

open Ast
open Cast
open Log
open Noincl_ast
open Typed_ast
open Cpp_sast
open Version

(* string -> Ast -> Noincl_ast -> Typed_ast -> Code_gen *)
let do_compile src_path bin_path keep_ast keep_il =
  let ast = noinclu_ast src_path in
  if keep_ast then
    let ast_path = bin_path ^ ".ast"
    and (fdefs, externs, exprs, tdefs) = ast in
    Out_channel.write_all ast_path ~data:(string_of_prog_struc ([], fdefs, externs, exprs, tdefs))
  else ();
  let sast = sast_of_ast ast in
  let cast = cast_of_sast sast in
  let cpp = string_of_program cast in
  if keep_il then
    let il_path = bin_path ^ ".cpp" in
    Out_channel.write_all il_path ~data:cpp
  else ();
  let cpp_compile = "clang++ -Wall -pedantic -fsanitize=address -std=c++14 -O2 -xc++ - support.cpp" in
  let ch = Unix.open_process_out cpp_compile in
  Out_channel.output_string ch cpp;
  ignore(Unix.close_process_out ch)

let command =
  Command.basic
    ~summary:"Compiler for the 🎵 #️⃣  language"
    ~readme:(fun () -> "For more information, visit https://github.com/el2724/note-hashtag")
    Command.Spec.(
      empty
      (*+> anon (maybe ("filename" %: string))*)
      +> flag "-A" no_arg ~doc:" output internal representation (syntax tree)"
      +> flag "-c" (optional string) ~doc:"file.nh compile the specified file"
      +> flag "-o" (optional_with_default "a.out" string) ~doc:"file write output to the specified file"
      +> flag "-S" no_arg ~doc:" output intermediate language representation (C++)"
      +> flag "-v" no_arg ~doc:" print verbose debugging information"
      +> flag "-vv" no_arg ~doc:" print extra verbose debugging information"
    )
    ( (* Handler *)
      fun show_ast infile_path outfile_path show_il verbose1 verbose2 () ->
        let log_level = if verbose2 then Debug else if verbose1 then Info else Warn in
        Log.set_min_level log_level;
        Log.debug "Parsed command line options:";
        Log.debug "  minimum log_level: %s" (string_of_level log_level);
        Log.debug "  compiling file (empty for stdin): %s" (match infile_path with None -> "" | Some(s) -> s);
        Log.debug "  writing executable to: %s" outfile_path;
        Log.debug "  keep intermediate representation: %B" show_il;
        Log.debug "  keep syntax tree: %B" show_ast;
        try
          do_compile infile_path outfile_path show_ast show_il;
        (* Catch any unhandled exceptions to suppress the nasty-looking message *)
        with Failure(msg) -> Log.error "%s" msg; Log.debug "call stack:\n%s" (Printexc.get_backtrace ()); exit 1
    )

let _ =
  Command.run ~version:(Version.release ()) ~build_info:(Version.build ()) command;
  Log.info "scanner and parser ran without crashing";
