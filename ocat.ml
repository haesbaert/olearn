open Core.Std

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

let docat file nflag uflag bflag vflag eflag tflag sflag =
  let nflag = bflag || nflag in
  let vflag = eflag || tflag || vflag in
  let ochar = Out_channel.output_char Out_channel.stdout in
  let ostr = Out_channel.output_string Out_channel.stdout in
  let bufsize = 1024 * 8 in
  let ic = match file with
    | "-" -> In_channel.stdin
    | file -> In_channel.create file
  in
  let rec catloop ic lnum c prev gobble =
    match c with
    | None -> ()
    | Some c ->
      let ignline = sflag && gobble && c = '\n' && prev = '\n' in
      let gobble = prev = '\n' && c = '\n' in
      match ignline with
      | true -> catloop ic lnum (In_channel.input_char ic) c gobble
      | _ ->
        let supbln = bflag && c = '\n' && prev = '\n' in (* supress blanks *)
        (* Should we print the line preamble *)
        if nflag && prev = '\n' && not supbln then
          ostr (sprintf "%6d\t" lnum);
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
        if uflag then Out_channel.(flush stdout);
        catloop ic nlnum (In_channel.input_char ic) c gobble
  in
  let rec rawcatloop ic buf =
    match In_channel.input ic ~pos:0 ~buf:buf ~len:bufsize with
    | 0 -> ()
    | len -> Out_channel.output Out_channel.stdout ~buf:buf ~pos:0 ~len:len;
      if uflag then Out_channel.(flush stdout);
      rawcatloop ic buf
  in
  match bflag, nflag, vflag, sflag with
  | false, false, false, false -> rawcatloop ic (Bytes.create bufsize)
  | _ -> catloop ic 1 (In_channel.input_char ic) '\n' false

let spec =
  let open Command.Spec in
  empty
  +> flag "-b" no_arg ~doc:" escape empty lines, implies -n"
  +> flag "-n" no_arg ~doc:" print line numbers"
  +> flag "-u" no_arg ~doc:" unbuffered output"
  +> flag "-v" no_arg ~doc:" display non printable characters"
  +> flag "-e" no_arg ~doc:" implies -v, and display a $ in the EOL"
  +> flag "-t" no_arg ~doc:" implies -v, and display tabs as ^I"
  +> flag "-s" no_arg ~doc:" squeeze adjacent newlines"
  +> anon (maybe_with_default "-" ("filename" %: string))

let command =
  Command.basic
    ~summary:"basic cat implementation."
    spec
    (fun bflag nflag uflag vflag eflag tflag sflag filename () ->
       docat filename nflag uflag bflag vflag eflag tflag sflag)

let () =
  Command.run ~version:"1.0" command
