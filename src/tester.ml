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
  let retval_pass = Result.Ok( () ) in
  (* Compile and check return value *)
  let nhc = Unix.open_process_in ("./nhc.native -c " ^ file) in
  print_string (In_channel.input_all nhc); (* No good way to do this -- just have to redirect stdout manually *)
  let nhc_retval = Unix.close_process_in nhc in
  if nhc_retval <> retval_pass then false else
  (* Run executable and check return value *)
  let child = Unix.open_process_in "./a.out" in
  let output = In_channel.input_all child
  and child_retval = Unix.close_process_in child in
  if child_retval <> retval_pass then false else
  (* Check child's output *)
  let expected = In_channel.read_all (Filename.chop_extension file ^ ".out") in
  output = expected

let test_files files =
  let print_test_file path =
    let passed = test_file path in
    (if passed then Log.debug "✅  %s" else Log.debug "❎  %s") (Filename.basename path);
    passed
  in
  List.map files ~f:print_test_file

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
  let results = test_files tests in
  let num_passed = List.count results ~f:(fun pass -> pass) in
  Log.info "Passed %d of %d tests" num_passed (List.length results)
