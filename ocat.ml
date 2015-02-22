open Core.Std

let buf_size = 4096

let rec copyloop ic oc buf =
  let c = In_channel.input ic ~pos:0 ~buf:buf ~len:buf_size in
  match c with
  | 0 -> ()
  | _ -> Out_channel.output oc ~pos:0 ~buf:buf ~len:c;
    copyloop ic oc buf

let do_cat file =
  let ic = match file with
  | None | Some "-" -> In_channel.stdin
  | Some file -> In_channel.create file in
  copyloop ic Out_channel.stdout (Bytes.create buf_size);
  In_channel.close ic

let spec =
  let open Command.Spec in
  empty
  +> anon (maybe ("filename" %: string))

let command =
  Command.basic
    ~summary:"basic cat implementation."
    spec
    (fun filename () -> do_cat filename)

let () =
  Command.run ~version:"1.0" command
