open Lwt_result.Infix

let open_err r =
  let open Lwt.Infix in
  r >|= Rresult.R.open_error_msg

let connect he ?port ~tls ?authenticator host =
  let port = match port with None -> if tls then 443 else 80 | Some p -> p in
  open_err (Happy_eyeballs_lwt.connect he host [port]) >>= fun (_addr, socket) ->
  if tls then
    Lwt_result.lift (match authenticator with
        | None -> Ca_certs.authenticator ()
        | Some a -> Ok a) >>= fun authenticator ->
    let config = Tls.Config.client ~authenticator () in
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
       Ok false
     else if String.equal proto "https:" then
       Ok true
     else
       Error (`Msg "unknown protocol")) >>= fun is_tls ->
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
    is_tls, user_pass, host, port, "/" ^ String.concat "/" path
  | _ -> Error (`Msg "couldn't decode URI on top")

let prep_headers headers host user_pass blen =
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

let single_request resolver ?config ?authenticator ~meth ~headers ?body uri =
  Lwt_result.lift (decode_uri uri) >>= fun (tls, user_pass, host, port, path) ->
  connect resolver ?port ~tls ?authenticator host >>= fun fd ->
  let blen = match body with None -> None | Some x -> Some (String.length x) in
  let headers = prep_headers headers host user_pass blen in
  let req = Httpaf.Request.create ~headers meth path in
  let finished, notify_finished = Lwt.wait () in
  let on_eof response data () =
    Lwt.wakeup_later notify_finished (Ok (response, data))
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
  let request_body =
    Httpaf_lwt_unix.Client.request ?config fd req ~error_handler ~response_handler
  in
  (match body with
   | Some body -> Httpaf.Body.write_string request_body body
   | None -> ());
  Httpaf.Body.close_writer request_body;
  finished

let one_request
    ?config
    ?authenticator
    ?(meth = `GET)
    ?(headers = Httpaf.Headers.empty)
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
      match resp.Httpaf.Response.status with
      | `Moved_permanently | `Found | `See_other | `Temporary_redirect ->
        (match Httpaf.Headers.get resp.Httpaf.Response.headers "location" with
         | Some uri ->
           Logs.debug (fun m -> m "following redirect to %s" uri);
           follow_redirect (pred count) uri
         | None -> Lwt_result.return (resp, body))
      | _ -> Lwt_result.return (resp, body)
  in
  follow_redirect max_redirect uri
