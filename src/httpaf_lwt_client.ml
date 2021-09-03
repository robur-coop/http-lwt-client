open Lwt_result.Infix

let open_err r =
  let open Lwt.Infix in
  r >|= Rresult.R.open_error_msg

let connect he ?port ?alpn_protocols ~tls ?authenticator host =
  let port = match port with None -> if tls then 443 else 80 | Some p -> p in
  open_err (Happy_eyeballs_lwt.connect he host [port]) >>= fun (_addr, socket) ->
  if tls then
    Lwt_result.lift (match authenticator with
        | None -> Ca_certs.authenticator ()
        | Some a -> Ok a) >>= fun authenticator ->
    let config = Tls.Config.client ?alpn_protocols ~authenticator () in
    Lwt.catch
      (fun () ->
         let host =
           let open Rresult.R.Infix in
           Rresult.R.to_option (Domain_name.of_string host >>= Domain_name.host)
         in
         Lwt_result.ok (Tls_lwt.Unix.client_of_fd config ?host socket))
      (fun e ->
         Lwt.return (Error (`Msg ("TLS failure: " ^ Printexc.to_string e))))
    >|= fun tls ->
    `Tls tls
  else
    Lwt.return (Ok (`Plain socket))

let decode_uri uri =
  let open Rresult.R.Infix in
  (* proto :// user : pass @ host : port / path *)
  match String.split_on_char '/' uri with
  | proto :: "" :: user_pass_host_port :: path ->
    (if String.equal proto "http:" then
       Ok ("http", false)
     else if String.equal proto "https:" then
       Ok ("https", true)
     else
       Error (`Msg "unknown protocol")) >>= fun (scheme, is_tls) ->
    let decode_user_pass up = match String.split_on_char ':' up with
      | [ user ; pass ] -> Ok (user, pass)
      | _ -> Error (`Msg "couldn't decode user and password")
    in
    (match String.split_on_char '@' user_pass_host_port with
     | [ host_port ] -> Ok (None, host_port)
     | [ user_pass ; host_port ] ->
       decode_user_pass user_pass >>= fun up ->
       Ok (Some up, host_port)
     | _ -> Error (`Msg "couldn't decode URI")) >>= fun (user_pass, host_port) ->
    (match String.split_on_char ':' host_port with
     | [] -> Error (`Msg "empty host part")
     | [ host ] -> Ok (host, None)
     | hd :: tl ->
       let port, host =
         match List.rev (hd :: tl) with
         | hd :: tl -> hd, String.concat ":" (List.rev tl)
         | _ -> assert false
       in
       (try Ok (host, Some (int_of_string port))
        with Failure _ -> Error (`Msg "couldn't decode port")))
    >>| fun (host, port) ->
    is_tls, scheme, user_pass, host, port, "/" ^ String.concat "/" path
  | _ -> Error (`Msg "couldn't decode URI on top")


let prep_http_1_1_headers headers host user_pass blen =
  let headers = Httpaf.Headers.of_list headers in
  let add = Httpaf.Headers.add_unless_exists in
  let headers = add headers "user-agent" ("httpaf-lwt-client/%%VERSION_NUM%%") in
  let headers = add headers "host" host in
  let headers = add headers "connection" "close" in
  let headers = match blen with
    | None -> headers
    | Some x -> add headers "content-length" (string_of_int x)
  in 
  match user_pass with
  | None -> headers
  | Some (user, pass) ->
    let data = Base64.encode_string (user ^ ":" ^ pass) in
    let s = "Basic " ^ data in
    add headers "authorization" s

let prep_h2_headers headers host user_pass blen =
  let headers = H2.Headers.of_list headers in
  let add = H2.Headers.add_unless_exists in
  let headers = add headers ":authority" host in
  let headers = match blen with
    | None -> add headers "content-length" "0"
    | Some x -> add headers "content-length" (string_of_int x)
  in 
  match user_pass with
  | None -> headers
  | Some (user, pass) ->
    let data = Base64.encode_string (user ^ ":" ^ pass) in
    let s = "Basic " ^ data in
    add headers "authorization" s

type response =
  | HTTP_1_1 of Httpaf.Response.t
  | H2 of H2.Response.t

let read_buffer_size = function
  | Some config -> Some config.Httpaf.Config.read_buffer_size
  | None -> None

let single_http_1_1_request ?config fd user_pass host meth path headers body =
  let blen = match body with None -> None | Some x -> Some (String.length x) in
  let headers = prep_http_1_1_headers headers host user_pass blen in
  let req = Httpaf.Request.create ~headers meth path in
  let finished, notify_finished = Lwt.wait () in
  let on_eof response data () =
    Lwt.wakeup_later notify_finished (Ok (HTTP_1_1 response, data))
  in
  let response_handler response response_body =
    let rec on_read on_eof data bs ~off ~len =
      let data = data ^ Bigstringaf.substring ~off ~len bs in
      Httpaf.Body.schedule_read response_body
        ~on_read:(on_read on_eof data)
        ~on_eof:(on_eof response (Some data))
    in
    Httpaf.Body.schedule_read response_body
      ~on_read:(on_read on_eof "")
      ~on_eof:(on_eof response None)
  in
  let error_handler e =
    let err = match e with
      | `Malformed_response x -> Error (`Msg ("malformed response: " ^ x))
      | `Invalid_response_body_length _ -> Error (`Msg "invliad response body length")
      | `Exn e -> Error (`Msg ("exception here: " ^ Printexc.to_string e))
    in
    Lwt.wakeup_later notify_finished err
  in
  let request_body, connection =
    Httpaf.Client_connection.request ?config req ~error_handler ~response_handler in
  Httpaf_lwt_unix.Client_HTTP_1_1.request ?read_buffer_size:(read_buffer_size config) fd connection ;
  (match body with
   | Some body -> Httpaf.Body.write_string request_body body
   | None -> ());
  Httpaf.Body.close_writer request_body;
  finished

let read_buffer_size = function
  | Some config -> Some config.H2.Config.read_buffer_size
  | None -> None

let single_h2_request ?config fd scheme user_pass host meth path headers body =
  let blen = match body with None -> None | Some x -> Some (String.length x) in
  let headers = prep_h2_headers headers host user_pass blen in
  let req = H2.Request.create ~scheme ~headers meth path in
  Logs.debug (fun m -> m "Sending @[<v 0>%a@]" H2.Request.pp_hum req);
  let finished, notify_finished = Lwt.wait () in
  let on_eof response data () =
    Lwt.wakeup_later notify_finished (Ok (H2 response, data))
  in
  let response_handler response response_body =
    let rec on_read on_eof data bs ~off ~len =
      let data = data ^ Bigstringaf.substring ~off ~len bs in
      H2.Body.schedule_read response_body
        ~on_read:(on_read on_eof data)
        ~on_eof:(on_eof response (Some data))
    in
    H2.Body.schedule_read response_body
      ~on_read:(on_read on_eof "")
      ~on_eof:(on_eof response None)
  in
  let error_handler e =
    let err = match e with
      | `Malformed_response x -> Error (`Msg ("malformed response: " ^ x))
      | `Invalid_response_body_length _ -> Error (`Msg "invliad response body length")
      | `Protocol_error (err, msg) -> Rresult.R.error_msgf "%a: %s" H2.Error_code.pp_hum err msg
      | `Exn e -> Error (`Msg ("exception here: " ^ Printexc.to_string e))
    in
    Lwt.wakeup_later notify_finished err
  in
  let connection =
    H2.Client_connection.create ?config ?push_handler:None ~error_handler in
  let request_body =
    H2.Client_connection.request connection req ~error_handler ~response_handler in
  Httpaf_lwt_unix.Client_H2.request ?read_buffer_size:(read_buffer_size config) fd connection ;
  (match body with
   | Some body -> H2.Body.write_string request_body body
   | None -> ());
  H2.Body.close_writer request_body;
  finished

let alpn_protocol = function
  | `Plain _ -> None
  | `Tls tls -> match Tls_lwt.Unix.epoch tls with
    | Ok { Tls.Core.alpn_protocol= Some "h2"; _ } -> Some `H2
    | Ok { Tls.Core.alpn_protocol= Some "http/1.1"; _ } -> Some `HTTP_1_1
    | Ok { Tls.Core.alpn_protocol= None; _ } -> None
    | Ok { Tls.Core.alpn_protocol= Some protocol; _ } ->
      Logs.warn (fun m -> m "The ALPN negociation gives a wrong protocol: %S." protocol) ;
      None
    | Error () -> None

let single_request resolver ?config ?authenticator ~meth ~headers ?body uri =
  let alpn_protocols = match config with
    | None -> None
    | Some (`HTTP_1_1 _) -> Some [ "http/1.1" ]
    | Some (`H2 _) -> Some [ "h2" ] in
  Lwt_result.lift (decode_uri uri) >>= fun (tls, scheme, user_pass, host, port, path) ->
  connect resolver ?port ?alpn_protocols ~tls ?authenticator host >>= fun fd ->
  match alpn_protocol fd, config with
  | (Some `HTTP_1_1 | None), Some (`HTTP_1_1 config) ->
    Logs.debug (fun m -> m "Start an http/1.1 connection as expected.") ;
    single_http_1_1_request ~config fd user_pass host meth path headers body
  | (Some `HTTP_1_1 | None), None ->
    Logs.debug (fun m -> m "Start an http/1.1 connection by default.") ;
    single_http_1_1_request fd user_pass host meth path headers body
  | (Some `H2 | None), Some (`H2 config) ->
    Logs.debug (fun m -> m "Start an h2 connection as expected.") ;
    single_h2_request ~config fd scheme user_pass host meth path headers body
  | Some `H2, None ->
    Logs.debug (fun m -> m "Start an h2 connection as requested by the server.") ;
    single_h2_request fd scheme user_pass host meth path headers body
  | Some `H2, Some (`HTTP_1_1 _config) ->
    Logs.warn (fun m -> m "Initiate an h2 connection despite a requested http/1.1 connection.") ;
    single_h2_request fd scheme user_pass host meth path headers body
  | Some `HTTP_1_1, Some (`H2 _config) ->
    Logs.warn (fun m -> m "Initiate an http/1.1 connection despite a requested h2 connection.") ;
    single_http_1_1_request fd user_pass host meth path headers body

let status = function
  | HTTP_1_1 resp -> (resp.Httpaf.Response.status :> H2.Status.t)
  | H2 resp -> resp.H2.Response.status

let location = function
  | HTTP_1_1 resp -> Httpaf.Headers.get resp.Httpaf.Response.headers "location"
  | H2 resp -> H2.Headers.get resp.H2.Response.headers "location"

let one_request
    ?config
    ?authenticator
    ?(meth = `GET)
    ?(headers = [])
    ?body
    ?(max_redirect = 5)
    uri
  =
  let he = Happy_eyeballs_lwt.create () in
  let rec follow_redirect count uri =
    if count = 0 then
      Lwt.return (Error (`Msg "redirect limit exceeded"))
    else
      single_request he ?config ?authenticator ~meth ~headers ?body uri
      >>= fun (resp, body) ->
      match status resp with
      | `Moved_permanently | `Found | `See_other | `Temporary_redirect ->
        (match location resp with
         | Some uri ->
           Logs.debug (fun m -> m "following redirect to %s" uri);
           follow_redirect (pred count) uri
         | None -> Lwt_result.return (resp, body))
      | _ -> Lwt_result.return (resp, body)
  in
  follow_redirect max_redirect uri
