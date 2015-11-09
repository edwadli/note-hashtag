open Unix
type filesystem = File of string | Directory of filesystem list

  let readdir_no_ex dirh =
    try
      Some (readdir dirh)
    with
      End_of_file -> None

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
    Directory (loop ());; 
let path = Sys.argv.(1) in
let fs = read_directory path in
print_endline (string_of_filesystem fs)
