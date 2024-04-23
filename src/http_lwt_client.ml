open Lwt_result.Infix

let src = Logs.Src.create "http_lwt_client" ~doc:"HTTP client"
module Log = (val Logs.src_log src : Logs.LOG)

let open_err r =
  let open Lwt.Infix in
  r >|= function Ok _ as r -> r | Error (`Msg _) as r -> r

let connect he ?port ?tls_config hostname =
  let port = match port, tls_config with
    | None, None -> 80
    | None, Some _ -> 443
    | Some p, _ -> p
  in
  open_err (Happy_eyeballs_lwt.connect he hostname [port]) >>= fun (_addr, socket) ->
  match tls_config with
  | Some config ->
    Lwt.catch
      (fun () ->
         Lwt_result.ok (Tls_lwt.Unix.client_of_fd config socket))
      (fun e ->
         Lwt.return (Error (`Msg ("TLS failure: " ^ Printexc.to_string e))))
    >|= fun tls ->
    `Tls tls
  | None -> Lwt.return (Ok (`Plain socket))

let decode_uri uri =
  (* proto :// user : pass @ host : port / path *)
  let ( >>= ) = Result.bind in
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
    >>= fun (host, port) ->
    Ok (is_tls, scheme, user_pass, host, port, "/" ^ String.concat "/" path)
  | _ -> Error (`Msg "couldn't decode URI on top")

let add_authentication ~add headers = function
  | None -> headers
  | Some (user, pass) ->
    let data = Base64.encode_string (user ^ ":" ^ pass) in
    let s = "Basic " ^ data in
    add headers "authorization" s

let user_agent = "http-lwt-client/%%VERSION_NUM%%"

let prep_http_1_1_headers headers host user_pass blen =
  let headers = Httpaf.Headers.of_list headers in
  let add = Httpaf.Headers.add_unless_exists in
  let headers = add headers "user-agent" user_agent in
  let headers = add headers "host" host in
  let headers = add headers "connection" "close" in
  let headers =
    add headers "content-length"
      (string_of_int (Option.value ~default:0 blen))
  in
  add_authentication ~add headers user_pass

let prep_h2_headers headers host user_pass blen =
  (* please note, that h2 (at least in version 0.10.0) encodes the headers
     in reverse order ; and for http/2 compatibility we need to retain the
     :authority pseudo-header first (after method/scheme/... that are encoded
     specially *)
  (* also note that "host" is no longer a thing, but :authority is -- so if
     we find a host header, we'll rephrase that as authority. *)
  let headers = List.rev_map (fun (k, v) -> (String.lowercase_ascii k, v)) headers in
  let headers = H2.Headers.of_rev_list headers in
  let headers, authority =
    match
      H2.Headers.get headers "host",
      H2.Headers.get headers ":authority"
    with
    | None, None -> headers, host
    | Some h, None ->
      Log.debug (fun m -> m "removing host header (inserting authority instead)");
      H2.Headers.remove headers "host", h
    | None, Some a ->
      H2.Headers.remove headers ":authority", a
    | Some h, Some a ->
      if String.equal h a then
        H2.Headers.remove (H2.Headers.remove headers ":authority") "host", h
      else begin
        Log.warn (fun m -> m "authority header %s mismatches host %s (keeping both)" a h);
        H2.Headers.remove headers ":authority", a
      end
  in
  let add hdr = H2.Headers.add_unless_exists hdr ?sensitive:None in
  let hdr = add H2.Headers.empty ":authority" authority in
  let hdr = H2.Headers.add_list hdr (H2.Headers.to_rev_list headers) in
  let hdr = add hdr "user-agent" user_agent in
  let hdr =
    add hdr "content-length"
      (string_of_int (Option.value ~default:0 blen))
  in
  add_authentication ~add hdr user_pass

module Version = Httpaf.Version
module Status = H2.Status
module Headers = H2.Headers

type response =
  { version : Version.t
  ; status  : Status.t
  ; reason  : string
  ; headers : Headers.t }

let pp_response ppf { version ; status ; reason ; headers } =
  Format.fprintf ppf "((version \"%a\") (status %a) (reason %S) (headers %a))"
    Version.pp_hum version Status.pp_hum status reason Headers.pp_hum headers

let update_keep_alive keep_alive response =
  let open Http_lwt_unix in
  match keep_alive with
  | None -> ()
  | Some r ->
     match Headers.get response.headers "Keep-Alive" with
     | None -> ()
     | Some ka ->
        match Stringext.find_from ka ~pattern:"max=" with
        | None -> ()
        | Some n ->
           try
             Scanf.sscanf (String.sub ka (n+4) (String.length ka - n - 4))
               "%d" (fun n -> r.max <- n)
           with _ ->
             Log.err (fun m -> m "Bad Keep-Alive header")

let single_http_1_1_request ?config ?keep_alive fd user_pass host meth path headers body f f_init =
  let blen = Option.map String.length body in
  let headers = prep_http_1_1_headers headers host user_pass blen in
  let req = Httpaf.Request.create ~headers meth path in
  let finished, notify_finished = Lwt.wait () in
  let w = ref false in
  let wakeup v =
    if !w then
      Log.err (fun m -> m "already woken up")
    else
      Lwt.wakeup_later notify_finished v;
    w := true
  in
  let on_eof response data () = wakeup (Ok (response, data))
  in
  let response_handler response response_body =
    let response : response =
      {
        version = response.Httpaf.Response.version ;
        status = (response.Httpaf.Response.status :> H2.Status.t) ;
        reason = response.Httpaf.Response.reason ;
        headers =
          H2.Headers.of_list
            (Httpaf.Headers.to_list response.Httpaf.Response.headers)
      }
    in
    update_keep_alive keep_alive response;
    let open Lwt.Infix in
    let rec on_read on_eof acc bs ~off ~len =
      (* XXX(dinosaure): we must do the copy before the [>>=].
         [Httpaf] will re-use [bs] then. *)
      let str = Bigstringaf.substring ~off ~len bs in
      let acc =
        acc >>= fun acc ->
        f response acc str
      in
      Httpaf.Body.schedule_read response_body
        ~on_read:(on_read on_eof acc)
        ~on_eof:(on_eof response acc)
    in
    let f_init = Lwt.return f_init in
    Httpaf.Body.schedule_read response_body
      ~on_read:(on_read on_eof f_init)
      ~on_eof:(on_eof response f_init)
  in
  let error_handler e =
    let err = match e with
      | `Malformed_response x -> Error (`Msg ("malformed response: " ^ x))
      | `Invalid_response_body_length _ -> Error (`Msg "invalid response body length")
      | `Exn e -> Error (`Msg ("exception here: " ^ Printexc.to_string e))
    in
    wakeup err
  in
  let request_body, connection =
    Httpaf.Client_connection.request ?config req ~error_handler ~response_handler
  in
  let read_buffer_size = match config with
    | Some config -> Some config.Httpaf.Config.read_buffer_size
    | None -> None
  in
  Http_lwt_unix.Client_HTTP_1_1.request ?read_buffer_size ?keep_alive fd connection ;
  (match body with
   | Some body -> Httpaf.Body.write_string request_body body
   | None -> ());
  Httpaf.Body.close_writer request_body;
  finished

let single_h2_request ?config ?keep_alive fd scheme user_pass host meth path headers body (f : response -> 'a -> string -> 'a Lwt.t) f_init =
  let blen = Option.map String.length body in
  let headers = prep_h2_headers headers host user_pass blen in
  let req = H2.Request.create ~scheme ~headers meth path in
  Log.debug (fun m -> m "Sending @[<v 0>%a@]" H2.Request.pp_hum req);
  let finished, notify_finished = Lwt.wait () in
  let w = ref false in
  let wakeup v =
    if !w then
      Log.err (fun m -> m "already woken up task")
    else
      Lwt.wakeup_later notify_finished v;
    w := true
  in
  let on_eof response data () = wakeup (Ok (response, data)) in
  let response_handler response response_body =
    let response : response = {
      version = { major = 2 ; minor = 0 } ;
      status = response.H2.Response.status ;
      reason = "" ;
      headers = response.H2.Response.headers ;
    } in
    update_keep_alive keep_alive response;
    let open Lwt.Infix in
    let rec on_read on_eof acc bs ~off ~len =
      (* XXX(dinosaure): we must do the copy before the [>>=].
         [H2] will re-use [bs] then. *)
      let str = Bigstringaf.substring bs ~off ~len in
      let acc =
        acc >>= fun acc ->
        f response acc str
      in
      H2.Body.Reader.schedule_read response_body
        ~on_read:(on_read on_eof acc)
        ~on_eof:(on_eof response acc)
    in
    let f_init = Lwt.return f_init in
    H2.Body.Reader.schedule_read response_body
      ~on_read:(on_read on_eof f_init)
      ~on_eof:(on_eof response f_init)
  in
  let error_handler e =
    let err = match e with
      | `Malformed_response x -> Error (`Msg ("malformed response: " ^ x))
      | `Invalid_response_body_length _ -> Error (`Msg "invalid response body length")
      | `Protocol_error (err, msg) ->
        let kerr _ = Error (`Msg (Format.flush_str_formatter ())) in
        Format.kfprintf kerr Format.str_formatter "%a: %s" H2.Error_code.pp_hum err msg
      | `Exn e -> Error (`Msg ("exception here: " ^ Printexc.to_string e))
    in
    Log.app (fun m -> m "here %s" (match err with Ok _ -> "ok" | Error `Msg m -> m));
    wakeup err
  in
  let connection =
    H2.Client_connection.create ?config ?push_handler:None ~error_handler ()
  in
  let request_body =
    H2.Client_connection.request connection req ~error_handler ~response_handler
  in
  let read_buffer_size = match config with
    | Some config -> Some config.H2.Config.read_buffer_size
    | None -> None
  in
  Http_lwt_unix.Client_H2.request ?read_buffer_size ?keep_alive fd connection;
  (match body with
   | Some body -> H2.Body.Writer.write_string request_body body
   | None -> ());
  H2.Body.Writer.close request_body;
  Lwt.finalize
    (fun () -> finished)
    (fun () ->
       H2.Client_connection.shutdown connection;
       Lwt.return_unit)

let alpn_protocol = function
  | `Plain _ -> None
  | `Tls tls -> match Tls_lwt.Unix.epoch tls with
    | Ok { Tls.Core.alpn_protocol= Some "h2"; _ } -> Some `H2
    | Ok { Tls.Core.alpn_protocol= Some "http/1.1"; _ } -> Some `HTTP_1_1
    | Ok { Tls.Core.alpn_protocol= None; _ } -> None
    | Ok { Tls.Core.alpn_protocol= Some protocol; _ } ->
      Log.warn (fun m -> m "The ALPN negotiation unexpectedly resulted in %S."
                    protocol);
      None
    | Error () -> None

type keep_alive = Http_lwt_unix.keep_alive

let new_keep_alive = Http_lwt_unix.new_keep_alive

let add_keep_alive headers = ("Connection", "Keep-Alive")::headers

let single_request resolver ?keep_alive ?config tls_config ~meth ~headers ?body uri f f_init =
  Lwt_result.lift (decode_uri uri) >>= fun (tls, scheme, user_pass, host, port, path) ->
  let connection =
    (if tls then
       Lwt_result.lift (Lazy.force tls_config) >|= function
       | `Custom c -> Some c
       | `Default config ->
          match Result.bind (Domain_name.of_string host) Domain_name.host with
          | Ok peer -> Some (Tls.Config.peer config peer)
          | Error _ -> Some config
     else
       Lwt_result.return None) >>= fun tls_config ->
    let open Http_lwt_unix in
    match keep_alive with
    | None -> connect resolver ?port ?tls_config host
              >>= (fun fd -> Lwt.return (Ok (fd, headers)))
    | Some ({ fd = None; _ } as r) ->
       let open Lwt.Infix in
       Http_lwt_unix.keep_alive_lock keep_alive >>=
         (fun () ->
           let open Lwt_result.Infix in
           connect resolver ?port ?tls_config host >>=
         (fun fd ->
           r.fd <- Some fd;
           Lwt.return (Ok (fd, add_keep_alive headers))))
    | Some { fd = Some fd; _ } ->
       Lwt.return (Ok (fd, add_keep_alive headers))
  in
  connection >>= fun (fd, headers) ->
  begin
    match alpn_protocol fd, config with
    | (Some `HTTP_1_1 | None), Some (`HTTP_1_1 config) ->
       Log.debug (fun m -> m "Start an http/1.1 connection as expected.");
       single_http_1_1_request ~config ?keep_alive fd user_pass host meth path headers body f f_init
    | (Some `HTTP_1_1 | None), None ->
       Log.debug (fun m -> m "Start an http/1.1 connection by default.");
       single_http_1_1_request ?keep_alive fd user_pass host meth path headers body f f_init
    | (Some `H2 | None), Some (`H2 config) ->
       Log.debug (fun m -> m "Start an h2 connection as expected.");
       single_h2_request ~config ?keep_alive fd scheme user_pass host meth path headers body f f_init
    | Some `H2, None ->
       Log.debug (fun m -> m "Start an h2 connection as requested by the server.");
       single_h2_request ?keep_alive fd scheme user_pass host meth path headers body f f_init
    | Some `H2, Some (`HTTP_1_1 _config) ->
       Log.warn (fun m -> m "Initiate an h2 connection despite a requested http/1.1 connection.");
       single_h2_request ?keep_alive fd scheme user_pass host meth path headers body f f_init
    | Some `HTTP_1_1, Some (`H2 _config) ->
       Log.warn (fun m -> m "Initiate an http/1.1 connection despite a requested h2 connection.");
       single_http_1_1_request ?keep_alive fd user_pass host meth path headers body f f_init
  end >>= fun (resp, body) ->
  Lwt.map (fun body -> Ok (resp, body)) body

let resolve_location ~uri ~location =
  match String.split_on_char '/' location with
  | "http:" :: "" :: _ -> Ok location
  | "https:" :: "" :: _ -> Ok location
  | "" :: "" :: _ ->
    let schema = String.sub uri 0 (String.index uri '/') in
    Ok (schema ^ location)
  | "" :: _ ->
    (match String.split_on_char '/' uri with
     | schema :: "" :: user_pass_host_port :: _ ->
       Ok (String.concat "/" [schema ; "" ; user_pass_host_port ^ location])
     | _ -> Error (`Msg ("expected an absolute uri, got: " ^ uri)))
  | _ -> Error (`Msg ("unknown location (relative path): " ^ location))

let default_auth = lazy (Ca_certs.authenticator ())

let request
    ?config
    ?tls_config
    ?authenticator
    ?(meth = `GET)
    ?(headers = [])
    ?body
    ?(max_redirect = 5)
    ?(follow_redirect = true)
    ?(happy_eyeballs = Happy_eyeballs_lwt.create ())
    ?keep_alive
    uri
    f f_init
  =
  let tls_config : ([`Custom of Tls.Config.client | `Default of Tls.Config.client ], [> `Msg of string ]) result Lazy.t =
    lazy
      (match tls_config with
       | Some cfg -> Ok (`Custom cfg)
       | None ->
         let alpn_protocols = match config with
            | None -> [ "h2" ; "http/1.1" ]
            | Some (`H2 _) -> [ "h2" ]
            | Some (`HTTP_1_1 _) -> [ "http/1.1" ]
         and auth = match authenticator with
           | None -> Lazy.force default_auth
           | Some a -> Ok a
         in
         Result.map
           (fun authenticator ->
              `Default (Tls.Config.client ~alpn_protocols ~authenticator ()))
           auth)
  in
  if not follow_redirect then
    single_request happy_eyeballs ?keep_alive ?config tls_config ~meth ~headers ?body
      uri f f_init
  else
    let rec follow_redirect count uri =
      if count = 0 then
        Lwt.return (Error (`Msg "redirect limit exceeded"))
      else
        single_request happy_eyeballs ?keep_alive ?config tls_config ~meth ~headers ?body uri f f_init
        >>= fun (resp, body) ->
        if Status.is_redirection resp.status then
          (match Headers.get resp.headers "location" with
           | Some location ->
              let open Lwt.Infix in
              Http_lwt_unix.reset_keep_alive ~close:true keep_alive >>=
                (fun () ->
                  let open Lwt_result.Infix in
                  Lwt_result.lift (resolve_location ~uri ~location) >>= fun uri ->
                  Log.debug (fun m -> m "following redirect to %s" uri);
                  follow_redirect (pred count) uri)
           | None -> Lwt_result.return (resp, body))
        else
          Lwt_result.return (resp, body)
    in
    follow_redirect max_redirect uri

let reset_keep_alive = Http_lwt_unix.reset_keep_alive
let active_keep_alive = function Some { Http_lwt_unix.fd = Some _; _ } -> true
                               | _ -> false
