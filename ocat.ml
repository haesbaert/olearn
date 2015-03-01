let iscntrl c =
  let c = (int_of_char c) in
  (c >= 0 && c < 32) || c = 127

let cntrlcharv c =
  let c = (int_of_char c) in
  if c = 127 then
    '?'
  else
    char_of_int (c lor 0o100)

let isascii c =
  (int_of_char c) <= 0o177

let toascii c =
  match isascii c with
  | true ->  (char_of_int ((int_of_char c) land 0o177))
  | false -> c

let catfiles bflag eflag nflag sflag tflag uflag vflag files =
  let nflag = bflag || nflag in
  let vflag = eflag || tflag || vflag in
  let ochar = output_char stdout in
  let ostr = output_string stdout in
  let bufsize = 1024 * 8 in
  let cat file =
    let ic = match file with
      | "-" -> stdin
      | file -> open_in file
    in
    let rec catloop ic lnum c prev gobble =
      let ignline = sflag && gobble && c = '\n' && prev = '\n' in
      let gobble = prev = '\n' && c = '\n' in
      match ignline with
      | true -> catloop ic lnum (input_char ic) c gobble
      | _ ->
        let supbln = bflag && c = '\n' && prev = '\n' in (* supress blanks *)
        (* Should we print the line preamble *)
        if nflag && prev = '\n' && not supbln then
          ostr (Printf.sprintf "%6d\t" lnum);
        let () = match vflag, isascii c, (iscntrl c) with
          | true, false, true ->
            ostr "M-^";
            ochar (cntrlcharv c);
            ochar (toascii c)
          | true, false, false ->
            ostr "M-";
            ochar (toascii c)
          | _ -> match c, eflag, tflag with
            | '\n', true, _    -> ostr "$\n"
            | '\t', _,    true -> ostr "^I"
            | _ -> ochar c
        in
        (* Should we increase the line number *)
        let nlnum = match c = '\n', supbln with
          | false, _ -> lnum
          | true, true -> lnum
          | true, false -> succ lnum
        in
        if uflag then flush stdout;
        catloop ic nlnum (input_char ic) c gobble
    in
    let rec rawcatloop ic buf =
      match input ic buf 0 bufsize with
      | 0 -> ()
      | len -> output stdout buf 0 len;
        if uflag then flush stdout;
        rawcatloop ic buf
    in
    try
      match bflag, nflag, vflag, sflag with
      | false, false, false, false ->
        rawcatloop ic (Bytes.create bufsize)
      | _ ->
        catloop ic 1 (input_char ic) '\n' false
    with End_of_file -> if ic <> stdin then close_in ic
  in
  (* cat main body *)
  match List.length files with
  | 0 -> cat "-"
  | _ -> List.iter cat files

(* Build Cmdliner command parser *)
open Cmdliner
let bflag = Arg.(value & flag & info ["b"] ~doc:"Escape empty lines, implies -n.")
let eflag = Arg.(value & flag & info ["e"] ~doc:"Implies -v, and display a $ in the EOL")
let nflag = Arg.(value & flag & info ["n"] ~doc:"Display non printable characters")
let sflag = Arg.(value & flag & info ["s"] ~doc:"Squeeze adjacent newlines")
let tflag = Arg.(value & flag & info ["t"] ~doc:"Implies -v, and display tabs as ^I")
let uflag = Arg.(value & flag & info ["u"] ~doc:"Print line numbers")
let vflag = Arg.(value & flag & info ["v"] ~doc:"Unbuffered output")
(* let files = Arg.(value & pos 0 string "-" & info [] ~docv:"FILENAME") *)
let files = Arg.(value & (pos_all string []) & info [] ~docv:"FILES")
let cmd =
  let doc = "cat clone in ocaml" in
  Term.(pure catfiles $ bflag $ eflag $ nflag $ sflag $ tflag $ uflag $ vflag $ files),
  Term.info "cat" ~version:"1.1" ~doc

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
