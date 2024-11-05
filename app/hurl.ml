let jump () protocol uri meth headers input output no_follow =
  let ( let* ) = Result.bind in
  let config = match protocol with
    | None -> None
    | Some `HTTP_1_1 -> Some (`HTTP_1_1 Httpaf.Config.default)
    | Some `H2 -> Some (`H2 H2.Config.default)
  in
  let open Lwt.Infix in
  let* body, default_meth =
    match input with
    | None -> Ok (None, `GET)
    | Some fn ->
      let* d = Bos.OS.File.read (Fpath.v fn) in
      Ok (Some d, `POST)
  in
  let meth = match meth with None -> default_meth | Some x -> x in
  Lwt_main.run (
    let fd, close = match output with
      | None -> Unix.stdout, fun () -> ()
      | Some fn ->
        if Sys.file_exists fn then
          invalid_arg ("output file " ^ fn ^ " already exists");
        let fd = Unix.openfile fn [ Unix.O_WRONLY; Unix.O_CREAT ] 0o644 in
        fd, fun () -> Unix.close fd
    in
    let reply _response () data =
      let bytes = Bytes.of_string data in
      let blen = String.length data in
      let written = Unix.write fd bytes 0 blen in
      if written <> blen then
        Printf.printf "couldn't fully write (%d of %d bytes)" written blen;
      Lwt.return_unit
    in
    (Http_lwt_client.request ?config ~meth ~headers ?body ~follow_redirect:(not no_follow) uri reply () >|= function
      | Ok (resp, ()) ->
        Format.fprintf Format.std_formatter "\n%a\n%!"
          Http_lwt_client.pp_response resp;
        Ok ()
      | Error `Msg msg as e ->
        Logs.err (fun m -> m "error %s" msg);
        e) >|= fun r ->
    close ();
    r)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let uri =
  let doc = "URL to retrieve" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"URL")

let output =
  let doc = "Save body output to a file" in
  Arg.(value & opt (some string) None & info [ "output" ] ~doc ~docv:"OUTPUT")

let input =
  let doc = "Upload a file as body" in
  Arg.(value & opt (some file) None & info [ "input" ] ~doc ~docv:"INPUT")

let header_c =
  let parse s = match String.split_on_char ':' s with
    | [ key ; value ] -> `Ok (key, String.trim value)
    | _ -> `Error "couldn't decode header"
  in
  (parse, fun ppf (a, b) -> Fmt.pf ppf "%s:%s" a b)

let header =
  let doc = "HTTP header (key:value)" in
  Arg.(value & opt_all header_c [] & info [ "header" ] ~doc ~docv:"HEADER")

let protocol =
  let vs =
    [ Some `HTTP_1_1, Arg.info [ "http1" ] ~doc:"Only use the HTTP/1.1 protocol"
    ; Some `H2, Arg.info [ "http2" ] ~doc:"Only use the H2 protocol" ]
  in
  Arg.(value & vflag None vs)

let meth =
  let doc = "HTTP method to use (defaults to GET, POST when body)" in
  let ms = [
    ("get", `GET) ;
    ("head", `HEAD) ;
    ("post", `POST) ;
    ("put", `PUT) ;
    ("delete", `DELETE)
  ] in
  Arg.(value & opt (some (enum ms)) None & info [ "method" ] ~doc ~docv:"METHOD")

let not_follow_redirect =
  let doc = "Do not follow HTTP redirects" in
  Arg.(value & flag (info ~doc [ "no-follow" ]))

let cmd =
  let term =
    Term.(term_result (const jump $ setup_log $ protocol $ uri $ meth $ header $ input $ output $ not_follow_redirect))
  and info = Cmd.info "hurl" ~version:"%%VERSION_NUM%%"
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
