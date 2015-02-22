open Core.Std

let rec copyloop ic oc buf =
  let c = In_channel.input ic ~pos:0 ~buf:buf ~len:4096 in
  match c with
  | 0 -> ()
  | _ -> Out_channel.output oc ~pos:0 ~buf:buf ~len:c;
    copyloop ic oc buf

let do_cat file =
  let ic = In_channel.create file in
  copyloop ic Out_channel.stdout (Bytes.create 4096);
  In_channel.close ic

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"basic cat implementation."
    spec
    (fun filename () -> do_cat filename)

let () =
  Command.run ~version:"omg" command
