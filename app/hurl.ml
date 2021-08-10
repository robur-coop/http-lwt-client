
let jump () uri meth header output input =
  let open Rresult.R.Infix in
  (match input with
   | None -> Ok (None, `GET)
   | Some fn -> Bos.OS.File.read (Fpath.v fn) >>| fun d -> (Some d, `POST))
  >>= fun (body, default_meth) ->
  let meth = match meth with None -> default_meth | Some x -> x in
  let headers =
    List.fold_left (fun hdr (k, v) -> Httpaf.Headers.add hdr k v)
      Httpaf.Headers.empty header
  in
  let open Lwt.Infix in
  Lwt_main.run (
    Httpaf_lwt_client.one_request ~meth ~headers ?body uri >|= function
    | Ok (resp, body) ->
      Format.fprintf Format.std_formatter "%a\n%!" Httpaf.Response.pp_hum resp;
      (match body with
       | None -> Ok ()
       | Some data ->
         match output with
         | None -> Format.fprintf Format.std_formatter "%s\n%!" data ; Ok ()
         | Some fn -> Bos.OS.File.write (Fpath.v fn) data)
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

let cmd =
  Term.(term_result (const jump $ setup_log $ uri $ meth $ header $ input $ output)),
  Term.info "hurl" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
