open Unix
open Printf
type filesystem = File of string | Directory of filesystem list

  let readdir_no_ex dirh =
    try
      Some (readdir dirh)
    with
      End_of_file -> None

let rec read_out ch l s =
    try
        while true do
	    let line = input_line ch in	    
		let l = (l ^ line) in
			read_out ch l s
        done
    with End_of_file ->
	ignore(close_in ch);
	let l = (l^"\n") in
	if l = s then 
	    print_endline "TEST PASSED: OUTPUT MATCHES"
	else 
	    print_endline "TEST FAILED: OUTPUT DIFFERS"
	|_ -> ()
	
	

let rec run_file f =
    let read_file = open_in (String.sub f 0 (String.length f - 3) ^ ".out") in
    let n = in_channel_length read_file in
    let s = Bytes.create n in
    really_input read_file s 0 n;
    close_in read_file;
    let compiler = "../src/nhc.native " ^ "-c " ^ f in
    ignore(Sys.command compiler);
    let exec = (String.sub f 0 (String.length f - 3)) ^ ".native" in
    let l = ""  in
    let ch = Unix.open_process_in exec in
	read_out ch l s

let rec check_file fs =
    match fs with
    | File f ->
	if String.sub f (String.length f - 3) 3  = ".nh" then begin
	    print_endline f;
	    ignore(run_file f)
	end
 	else
	   () 
    | Directory d ->
	List.iter check_file d	

let rec string_of_filesystem fs =
    match fs with
    | File filename -> filename ^ "\n"
    | Directory fs_list ->
        List.fold_left (^) "" (List.map string_of_filesystem fs_list)

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
          let this = if stat.st_kind = S_DIR then
                       read_directory pathname
                     else
                       File pathname in
          this :: loop () in
    		Directory(loop ());; 
       
 
let path = Sys.argv.(1) in
let fs = read_directory path in
check_file fs
