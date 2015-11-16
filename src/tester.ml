open Core.Std
open Filename
open Printf
open Unix

open Log

type filesystem = File of string | Directory of filesystem list

let rec read_out ch l =
  try
    let l = l ^ input_line ch in read_out ch l
  with End_of_file ->
    ignore (In_channel.close ch); l

let test_file file =
  ignore (Sys.command ("./nhc.native " ^ "-c " ^ file));
  let child = Unix.open_process_in "./a.out" in
  let output = In_channel.input_all child
  and retval = Unix.close_process_in child
  and expected = In_channel.read_all (Filename.chop_extension file ^ ".out") in
  let passed = (output = expected && retval = Result.Ok( () )) in
  (if passed then Log.debug "✅  %s" else Log.debug "❎  %s") (Filename.basename file); passed

let test_files files =
  List.map files ~f:test_file

let rec filenames_of_filesystem fs =
  match fs with
  | File(filename) -> if Filename.check_suffix filename ".nh" then [ filename ] else []
  | Directory(fs_list) -> List.fold_left (List.map fs_list ~f:filenames_of_filesystem) ~init:[] ~f:(@)

let readdir_no_ex dirh =
  try
    Some (readdir dirh)
  with
    End_of_file -> None

let rec read_directory path =
  let dirh = opendir path in
  let rec loop () =
    let filename = readdir_no_ex dirh in
    match filename with
    | None -> []
    | Some "." -> loop ()
    | Some ".." -> loop ()
    | Some filename ->
      let pathname = path ^ "/" ^ filename in
      let stat = lstat pathname in
      let this = if stat.st_kind = S_DIR then read_directory pathname else File pathname in
      this :: loop () in
      Directory(loop ());;

let _ =
  Log.set_min_level Debug;
  let path = Sys.argv.(1) in
  let fs = read_directory path in
  let tests = filenames_of_filesystem fs in
  print_endline (String.concat ~sep:"\n" tests);
  let results = test_files tests in
  let num_passed = List.count results ~f:(fun pass -> pass) in
  Log.info "Passed %d of %d tests" num_passed (List.length results)
