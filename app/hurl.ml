open Lwt.Infix

let jump () uri =
  Lwt_main.run (Httpaf_lwt_client.one_request uri >|= function
    | Ok (resp, body) ->
      Format.fprintf Format.std_formatter "%a\n%!" Httpaf.Response.pp_hum resp;
      (match body with None -> () | Some data ->
          Format.fprintf Format.std_formatter "%s\n%!" data);
      Ok ()
    | Error `Msg msg as e ->
      Logs.err (fun m -> m "error %s" msg);
      e)

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

let cmd =
  Term.(term_result (const jump $ setup_log $ uri)),
  Term.info "hurl" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
