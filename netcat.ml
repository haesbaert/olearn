open Lwt

let bufsize = 1024 * 16

(* XXX change this, conflits with Unix.environment *)
type environment = {
  host      : string;
  port      : string;
  listen    : bool;
  ipv4only  : bool;
  ipv6only  : bool;
  numeric   : bool;
  udp       : bool;
  verbose   : bool;
}

let cons_if p v tl =
  if p then
    v :: tl
  else
    tl

(* Wrapper for printing if verbose is on *)
let vprintf env fmt = if env.verbose then Lwt_io.eprintf fmt else
    Printf.ksprintf (fun _ -> Lwt.return ()) fmt
let vprintl env fmt = if env.verbose then Lwt_io.eprintl fmt else Lwt.return ()

let rec forward env buf desc_in desc_out =
  let open Lwt_unix in
  read desc_in buf 0 bufsize >>= fun rn ->
  if rn <= 0 then
    Lwt.return ()
  else
    write desc_out buf 0 rn >>= fun wn ->
    if wn <= 0 then
      Lwt_io.eprintl "out closed" >>= fun () ->
      Lwt.return ()
    else begin

      assert (rn = wn);
      forward env buf desc_in desc_out;
    end

let rec wait_conn env lsock =
  let open Lwt_unix in
  accept lsock >>= fun (netsock, saddr) ->
  getnameinfo saddr [] >>= fun ni ->
  vprintf env "connection from %s:%s\n" ni.ni_hostname ni.ni_service >>= fun () ->
  let net_to_stdout = forward env (Bytes.create bufsize) netsock stdout in
  let stdin_to_net = forward env (Bytes.create bufsize) stdin netsock in
  net_to_stdout >>= fun () -> close netsock >>= fun () ->
  net_to_stdout >>= fun () -> ignore (vprintl env "net closed");
  stdin_to_net >>= fun () -> ignore (vprintl env "stdin closed");
  (* ignore (Lwt.finalize (fun () -> net_to_stdout) (fun () -> Lwt_io.eprintl "STDIN FECHOU")); *)
  Lwt.choose [ net_to_stdout; stdin_to_net ] >>= fun () ->
  vprintl env "connection finished"

let make_lsock ai =
  let open Lwt_unix in
  let sock = socket ai.ai_family ai.ai_socktype ai.ai_protocol in
  setsockopt sock SO_REUSEADDR true;
  bind sock ai.ai_addr;
  listen sock 10;
  sock

let rec get_one_lsock = function
  | [] -> failwith "No addresses to listen."
  | ai :: tl -> try (make_lsock ai) with _ -> get_one_lsock tl

let serve env =
  let open Lwt_unix in
  let opts =
    cons_if (env.ipv4only || (not env.ipv4only && not env.ipv6only))
      (AI_FAMILY PF_INET)
    @@ cons_if env.ipv6only (AI_FAMILY PF_INET6)

    @@ cons_if env.numeric AI_NUMERICHOST
    @@ cons_if env.udp (AI_SOCKTYPE SOCK_DGRAM)
    @@ cons_if (not env.udp) (AI_SOCKTYPE SOCK_STREAM)
    @@ [AI_PASSIVE]
  in
  getaddrinfo env.host env.port opts >>= fun ais ->
  wait_conn env (get_one_lsock ais)

let do_netcat env =
  if env.listen then
    Lwt_main.run (serve env);
  `Ok ()

(* XXX this is horrible, make it better *)
let args_to_host_port listen arg0 arg1 =
  if listen then match arg0, arg1 with
    | Some arg0, Some arg1 -> (arg0, arg1)
    | Some arg0, None      -> ("", arg0)
    | _ -> raise (Invalid_argument "Invalid positional arguments")
  else match arg0, arg1 with
    | Some arg0, Some arg1 -> (arg0, arg1)
    | _ ->
      raise (Invalid_argument "Invalid positional arguments")

let make_env listen ipv4only ipv6only numeric udp verbose arg0 arg1 =
  if ipv4only && ipv6only then
    `Error (true, "options -4 and -6 are mutually exclusive")
  else
    try
      let host, port = args_to_host_port listen arg0 arg1 in
      `Ok {host; port; listen; ipv4only; ipv6only; numeric ; udp; verbose }
    with
      e -> `Error (true, Printexc.to_string e)

let netcat listen ipv4only ipv6only numeric udp verbose arg0 arg1 =
  let env = (make_env listen ipv4only ipv6only
               numeric udp verbose arg0 arg1) in
  match env with
  | `Ok env ->
    let r = do_netcat env in
    r
  | `Error (t, e) -> `Error (t, e)

open Cmdliner
let listen = Arg.(value & flag & info ["l"] ~doc:"Listen.")
let ipv4only = Arg.(value & flag & info ["4"] ~doc:"IPv4 only.")
let ipv6only = Arg.(value & flag & info ["6"] ~doc:"IPv6 only.")
let numeric = Arg.(value & flag & info ["n"] ~doc:"Numeric hosts only.")
let udp = Arg.(value & flag & info ["u"] ~doc:"Use udp instead of tcp.")
let verbose = Arg.(value & flag & info ["v"] ~doc:"verbose.")
let arg0 = Arg.(value & (pos 0 (some string) None) & info [] ~docv:"host")
let arg1 = Arg.(value & (pos 1 (some string) None) & info [] ~docv:"port")

let cmd =
  let doc = "netcat clone in ocaml" in
  Term.(ret (pure netcat $ listen $ ipv4only $ ipv6only $ numeric $ udp $
             verbose $ arg0 $ arg1)),
  Term.info "netcat" ~version:"1.0" ~doc

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
