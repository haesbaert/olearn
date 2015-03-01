open Printf

let isspace c =
  let c = (int_of_char c) in
  (c >= 9 && c <= 13) || c = 32

let countword c inspc w =
  match isspace c, inspc with
  | false, true -> succ w
  | _           -> w

let countline c l = match c with
  | '\n' -> succ l
  | _    -> l

let wcfiles cflag lflag wflag files =
  let printall = cflag = false && lflag = false && wflag = false in
  let wc file =
    let ic = match file with
      | "-" -> stdin
      | _ -> open_in file
    in
    let rec wcloop lines words bytes inspc =
      let c = try Some (input_char ic)
        with End_of_file -> None
      in
      match c with
      | None -> (lines, words, bytes)
      | Some c -> wcloop (countline c lines)
                    (countword c inspc words)
                    (succ bytes) (isspace c)
    in
    let (lines,words,bytes) = wcloop 0 0 0 true in
    if lflag || printall then
      printf "%8d" lines;
    if wflag || printall then
      printf "%8d" words;
    if cflag || printall then
      printf "%8d" bytes;
    if ic <> stdin then
      (close_in ic; printf " %s" file);
    print_newline ()
  in
  (* main body *)
  match List.length files with
  | 0 -> wc "-"
  | _ -> List.iter wc files


open Cmdliner
let cflag = Arg.(value & flag & info ["c" ; "m"]
                   ~doc:"Print the number of bytes in each input file.")
let lflag = Arg.(value & flag & info ["l"] ~doc:"Print the number of lines.")
let wflag = Arg.(value & flag & info ["w"] ~doc:"Print the number of words.")
let files = Arg.(value & (pos_all string []) & info [] ~docv:"FILES")

let cmd =
  let doc = "wc clone in ocaml" in
  Term.(pure wcfiles $ cflag $ lflag $ wflag $ files),
  Term.info "wc" ~version:"1.0" ~doc
let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
