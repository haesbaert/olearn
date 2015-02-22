open Core.Std

let buf_size = 4096

let rec copyloop ic oc buf uflag =
  let c = In_channel.input ic ~pos:0 ~buf:buf ~len:buf_size
  in match c with
  | 0 -> ()
  | _ -> Out_channel.output oc ~pos:0 ~buf:buf ~len:c;
    if uflag = true then
      Out_channel.flush oc;
    copyloop ic oc buf uflag

let rec copylines ic oc buf linenum uflag bflag =
  (* Check if we should escape the line in question *)
  let escape line bflag = bflag && String.length line = 0 in
  let line = In_channel.input_line ic in
  match line with
    | None -> ()
    | Some line -> match escape line bflag with
      | true -> copylines ic oc buf linenum uflag bflag
      | false -> Out_channel.output_string oc (sprintf "%6d  %s\n" linenum line);
        if uflag = true then
          Out_channel.flush oc;
        copylines ic oc buf (linenum + 1) uflag bflag

let do_cat file nflag uflag bflag =
  let nflag = if bflag = true then true else nflag in
  let oc = Out_channel.stdout in
  let buf = Bytes.create buf_size in
  let ic = match file with
    | "-" -> In_channel.stdin
    | file -> In_channel.create file
  in
  let () = match nflag with
    | false -> copyloop ic oc buf uflag
    | true  -> copylines ic oc buf 1 uflag bflag
  in
  In_channel.close ic

let spec =
  let open Command.Spec in
  empty
  +> flag "-b" no_arg ~doc:" escape empty lines, implies -n"
  +> flag "-n" no_arg ~doc:" print line numbers"
  +> flag "-u" no_arg ~doc:" unbuffered output"
  +> anon (maybe_with_default "-" ("filename" %: string))

let command =
  Command.basic
    ~summary:"basic cat implementation."
    spec
    (fun bflag nflag uflag filename () -> do_cat filename nflag uflag bflag)

let () =
  Command.run ~version:"1.0" command
