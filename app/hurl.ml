let jump () protocol uri meth headers output input no_follow =
  let ( let* ) = Result.bind in
  let config = match protocol with
    | None -> None
    | Some `HTTP_1_1 -> Some (`HTTP_1_1 Httpaf.Config.default)
    | Some `H2 -> Some (`H2 H2.Config.default)
  in
  let* body, default_meth =
    match input with
    | None -> Ok (None, `GET)
    | Some fn ->
      let* d = Bos.OS.File.read (Fpath.v fn) in
      Ok (Some d, `POST)
  in
  let meth = match meth with None -> default_meth | Some x -> x in
  let open Lwt.Infix in
  Lwt_main.run ((
    Http_lwt_client.one_request ?config ~meth ~headers ?body ~follow_redirect:(not no_follow) uri >|= function
    | Ok (resp, body) ->
      Format.fprintf Format.std_formatter "%a\n%!"
        Http_lwt_client.pp_response resp;
      (match body with
       | None -> Ok ()
       | Some data ->
         match output with
         | None -> Format.fprintf Format.std_formatter "%s\n%!" data ; Ok ()
         | Some fn -> Bos.OS.File.write (Fpath.v fn) data)
    | Error `Msg msg as e ->
      Logs.err (fun m -> m "error %s" msg);
      e) >|= fun r ->
     Unix.sleep 100;
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
  Arg.(value & opt (some file) None & info [ "output" ] ~doc ~docv:"OUTPUT")

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
