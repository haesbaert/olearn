open Core.Std

let buf_size = 4096

let rec copyloop ic oc buf =
  let c = In_channel.input ic ~pos:0 ~buf:buf ~len:buf_size
  in match c with
  | 0 -> ()
  | _ -> Out_channel.output oc ~pos:0 ~buf:buf ~len:c;
    copyloop ic oc buf

let rec copylines ic oc buf linenum =
  let line = In_channel.input_line ic
  in match line with
    | None -> ()
    | Some line -> Out_channel.output_string oc (sprintf "%6d  %s\n" linenum line);
      copylines ic oc buf (linenum + 1)

let do_cat file nflag =
  let oc = Out_channel.stdout in
  let buf = Bytes.create buf_size in
  let ic = match file with
    | "-" -> In_channel.stdin
    | file -> In_channel.create file
  in
  let () = match nflag with
    | false -> copyloop ic oc buf
    | true  -> copylines ic oc buf 1
  in
  In_channel.close ic

let spec =
  let open Command.Spec in
  empty
  +> flag "-n" no_arg ~doc:" print line numbers"
  +> anon (maybe_with_default "-" ("filename" %: string))

let command =
  Command.basic
    ~summary:"basic cat implementation."
    spec
    (fun nflag filename () -> do_cat filename nflag)

let () =
  Command.run ~version:"1.0" command
