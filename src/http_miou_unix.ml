module type RUNTIME = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  val yield_reader : t -> (unit -> unit) -> unit

  val next_write_operation :
    t -> [ `Write of Bigstringaf.t Faraday.iovec list | `Close of int | `Yield ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val yield_writer : t -> (unit -> unit) -> unit
  val report_exn : t -> exn -> unit
end

module Buffer : sig
  type t

  val create : int -> t
  val get : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
  val put : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
end = struct
  type t = {
    mutable buffer : Bigstringaf.t;
    mutable off : int;
    mutable len : int;
  }

  let create size =
    let buffer = Bigstringaf.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0 then begin
      t.off <- 0;
      t.len <- 0
    end
    else if t.off > 0 then begin
      Bigstringaf.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
      t.off <- 0
    end

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0 then t.off <- 0;
    n

  let put t ~f =
    compress t;
    let off = t.off + t.len in
    let buf = t.buffer in
    if Bigstringaf.length buf = t.len then begin
      t.buffer <- Bigstringaf.create (2 * Bigstringaf.length buf);
      Bigstringaf.blit buf ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len
    end;
    let n = f t.buffer ~off ~len:(Bigstringaf.length t.buffer - off) in
    t.len <- t.len + n;
    n
end

let src = Logs.Src.create "http-miou-unix"

module Log = (val Logs.src_log src : Logs.LOG)

let catch ~on fn =
  try fn ()
  with exn ->
    Log.err (fun m ->
        m "Got an unexpected exception: %S" (Printexc.to_string exn));
    on exn

exception Flow of string

module type S = sig
  type conn
  type flow

  type protect = {
    protect : 'a 'b. orphans:unit Miou.orphans -> ('a -> 'b) -> 'a -> 'b;
  }
  [@@unboxed]

  val run :
    conn ->
    ?give:Miou.Ownership.t list ->
    ?disown:(flow -> unit) ->
    read_buffer_size:int ->
    flow ->
    protect * unit Miou.t
end

let rec terminate orphans =
  match Miou.care orphans with
  | None -> ()
  | Some None ->
      Miou.yield ();
      terminate orphans
  | Some (Some prm) ->
      Miou.await_exn prm;
      terminate orphans

module Make (Flow : Flow.S) (Runtime : RUNTIME) :
  S with type conn = Runtime.t and type flow = Flow.t = struct
  type conn = Runtime.t
  type flow = Flow.t

  let recv flow buffer =
    let bytes_read =
      Buffer.put buffer ~f:(fun bstr ~off:dst_off ~len ->
          match Flow.read ~read_buffer_size:len flow with
          | Ok (`Data { Cstruct.buffer; off = src_off; len }) ->
              Bigstringaf.blit buffer ~src_off bstr ~dst_off ~len;
              len
          | Ok `End_of_input -> 0
          | Error err ->
              Log.debug (fun m -> m "close the socket (recv)");
              Flow.close flow;
              raise (Flow (Fmt.str "%a" Flow.pp_error err)))
    in
    if bytes_read = 0 then `Eof else `Ok bytes_read

  let writev flow bstrs =
    let copy { Faraday.buffer; off; len } = Bigstringaf.copy buffer ~off ~len in
    let css = List.map copy bstrs |> List.map Cstruct.of_bigarray in
    match Flow.writev flow css with
    | Ok () ->
        let len = List.fold_left (fun a { Cstruct.len; _ } -> a + len) 0 css in
        `Ok len
    | Error err ->
        Log.err (fun m -> m "got an error: %a" Flow.pp_error err);
        Log.debug (fun m -> m "close the socket (writev)");
        Flow.close flow;
        `Closed

  type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t

  let launch ?give ?orphans fn k =
    let _prm = Miou.call_cc ?orphans ?give fn in
    Effect.Deep.continue k ()

  (* Protected runtime operations.


   *)

  let protect ?give ?orphans fn v =
    let retc = Fun.id in
    let exnc = raise in
    let open Effect.Deep in
    let effc : type c. c Effect.t -> ((c, 'b) continuation -> 'b) option =
      function
      | Spawn fn -> Some (launch ?give ?orphans fn)
      | _ -> None
    in
    match_with fn v { retc; exnc; effc }

  let next_read_operation ?give ?orphans =
    protect ?give ?orphans Runtime.next_read_operation

  let next_write_operation ?give ?orphans =
    protect ?give ?orphans Runtime.next_write_operation

  let read ?give ?orphans conn bstr ~off ~len =
    protect ?give ?orphans (Runtime.read conn ~off ~len) bstr

  let read_eof ?give ?orphans conn bstr ~off ~len =
    protect ?give ?orphans (Runtime.read_eof conn ~off ~len) bstr

  let report_exn ?give ?orphans conn exn =
    Log.err (fun m -> m "report an exception: %S" (Printexc.to_string exn));
    protect ?give ?orphans (Runtime.report_exn conn) exn

  let report_write_result ?give ?orphans conn =
    protect ?give ?orphans (Runtime.report_write_result conn)

  let yield_reader ?give ?orphans conn =
    protect ?give ?orphans (Runtime.yield_reader conn)

  let yield_writer ?give ?orphans conn =
    protect ?give ?orphans (Runtime.yield_writer conn)

  type protect = {
    protect : 'a 'b. orphans:unit Miou.orphans -> ('a -> 'b) -> 'a -> 'b;
  }
  [@@unboxed]

  let run conn ?(give = []) ?(disown = Fun.const ()) ~read_buffer_size flow =
    let buffer = Buffer.create read_buffer_size in

    let rec reader () =
      let rec go orphans () =
        match next_read_operation ~orphans ~give conn with
        | `Read -> (
            Log.debug (fun m -> m "next read operation: `read");
            let read_eof = read_eof ~orphans ~give in
            let read = read ~orphans ~give in
            match recv flow buffer with
            | `Eof ->
                Buffer.get buffer ~f:(fun bstr ~off ~len ->
                    read_eof conn bstr ~off ~len)
                |> ignore;
                go orphans ()
            | `Ok _ ->
                Buffer.get buffer ~f:(fun bstr ~off ~len ->
                    read conn bstr ~off ~len)
                |> ignore;
                go orphans ())
        | `Yield ->
            Log.debug (fun m -> m "next read operation: `yield");
            let continuation () = Effect.perform (Spawn reader) in
            yield_reader conn ~orphans ~give continuation;
            disown flow;
            terminate orphans
        | `Close ->
            Log.debug (fun m -> m "read: disown the file-descriptor");
            disown flow;
            terminate orphans
      in
      let orphans = Miou.orphans () in
      catch ~on:(report_exn conn ~orphans ~give) @@ fun () -> go orphans ()
    in
    let rec writer () =
      let rec go orphans () =
        match next_write_operation ~orphans ~give conn with
        | `Write iovecs ->
            Log.debug (fun m -> m "next write operation: `write");
            writev flow iovecs |> report_write_result conn ~orphans ~give;
            go orphans ()
        | `Yield ->
            Log.debug (fun m -> m "next write operation: `yield");
            let continuation () = Effect.perform (Spawn writer) in
            yield_writer conn ~orphans ~give continuation;
            disown flow;
            terminate orphans
        | `Close _ ->
            Log.debug (fun m -> m "next write operation: `close");
            (* TODO(dinosaure): something already closed the socket when we use http/1.1 *)
            Flow.shutdown flow `Send;
            terminate orphans
      in
      let orphans = Miou.orphans () in
      catch ~on:(report_exn conn ~orphans ~give) @@ fun () -> go orphans ()
    in
    let protect ~orphans = protect ~orphans ~give in
    let prm =
      Miou.call_cc ~give @@ fun () ->
      let p0 = Miou.call_cc ~give reader in
      let p1 = Miou.call_cc ~give writer in
      let result =
        match Miou.await_all [ p0; p1 ] with
        | [ Ok (); Ok () ] -> Ok ()
        | [ Error exn; _ ] | [ _; Error exn ] -> Error exn
        | _ -> assert false
      in
      Log.debug (fun m -> m "close the file-descriptor");
      disown flow;
      match result with Ok () -> () | Error exn -> raise exn
    in
    Log.debug (fun m -> m "the main task is: %a" Miou.Promise.pp prm);
    ({ protect }, prm)
end

module TCP = struct
  type t = Miou_unix.file_descr
  type error = Unix.error * string * string

  let pp_error ppf (err, f, v) =
    Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)

  let read ?(read_buffer_size = 0x1000) flow =
    let buf = Bytes.create read_buffer_size in
    match Miou_unix.read flow buf ~off:0 ~len:(Bytes.length buf) with
    | 0 -> Ok `End_of_input
    | len -> Ok (`Data (Cstruct.of_string (Bytes.sub_string buf 0 len)))
    | exception Unix.Unix_error (Unix.ECONNRESET, _, _) -> Ok `End_of_input
    | exception Unix.Unix_error (err, f, v) -> Error (err, f, v)

  let write flow ({ Cstruct.len; _ } as cs) =
    let str = Cstruct.to_string cs in
    try Ok (Miou_unix.write flow str ~off:0 ~len)
    with Unix.Unix_error (err, f, v) -> Error (err, f, v)

  let writev flow css =
    let rec go = function
      | [] -> Ok ()
      | x :: r -> begin
          match write flow x with Ok () -> go r | Error _ as err -> err
        end
    in
    go css

  let close = Miou_unix.close

  let shutdown flow = function
    | `Recv ->
        Logs.debug (fun m -> m "shutdown the receiving side");
        Miou_unix.shutdown flow Unix.SHUTDOWN_RECEIVE
    | `Send ->
        Logs.debug (fun m -> m "shutdown the sending side");
        Miou_unix.shutdown flow Unix.SHUTDOWN_SEND
end

module TLS = Tls_miou.Make (TCP)

type tls = TLS.t
type tls_error = TLS.error

let pp_tls_error = TLS.pp_error
let to_tls = TLS.client_of_flow

let epoch tls =
  match tls.TLS.state with
  | `End_of_input | `Error _ -> None
  | `Active tls -> (
      match Tls.Engine.epoch tls with
      | `InitialEpoch -> assert false
      | `Epoch data -> Some data)

module Httpaf_Client_connection = struct
  include Httpaf.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

(* Implementations. *)

type config = [ `V1 of Httpaf.Config.t | `V2 of H2.Config.t ]
type flow = [ `Tls of TLS.t | `Tcp of Miou_unix.file_descr ]
type request = [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ]
type response = [ `V1 of Httpaf.Response.t | `V2 of H2.Response.t ]

type 'body body = {
  body : 'body;
  write_string : 'body -> ?off:int -> ?len:int -> string -> unit;
  close : 'body -> unit;
}

type ('resp, 'body) version =
  | V1 : (Httpaf.Response.t, [ `write ] Httpaf.Body.t body) version
  | V2 : (H2.Response.t, H2.Body.Writer.t body) version

type error =
  [ `V1 of Httpaf.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string ]

let pp_error ppf = function
  | `V1 (`Malformed_response msg) ->
      Fmt.pf ppf "Malformed HTTP/1.1 response: %s" msg
  | `V1 (`Invalid_response_body_length _resp) ->
      Fmt.pf ppf "Invalid response body length"
  | `V1 (`Exn exn) | `V2 (`Exn exn) ->
      Fmt.pf ppf "Got an unexpected exception: %S" (Printexc.to_string exn)
  | `V2 (`Malformed_response msg) -> Fmt.pf ppf "Malformed H2 response: %s" msg
  | `V2 (`Invalid_response_body_length _resp) ->
      Fmt.pf ppf "Invalid response body length"
  | `V2 (`Protocol_error (err, msg)) ->
      Fmt.pf ppf "Protocol error %a: %s" H2.Error_code.pp_hum err msg
  | `Protocol msg -> Fmt.string ppf msg

type ('resp, 'acc) await = unit -> ('resp * 'acc, error) result

type 'acc process =
  | Process :
      ('resp, 'body) version * ('resp, 'acc) await * 'body
      -> 'acc process

module A = Make (TLS) (Httpaf_Client_connection)
module B = Make (TCP) (Httpaf_Client_connection)
module C = Make (TLS) (H2.Client_connection)
module D = Make (TCP) (H2.Client_connection)

(* NOTE(dinosaure): we avoid first-class module here. *)
let run ~f acc config flow request =
  let response : response option ref = ref None
  and error = ref None
  and acc = ref acc in
  let error_handler err =
    Log.err (fun m -> m "Got an error: %a" pp_error err);
    match err with
    | `V1 (`Exn (Flow msg)) | `V2 (`Exn (Flow msg)) ->
        error := Some (`Protocol msg)
    | err -> error := Some err
  in
  let response_handler ?(shutdown = Fun.const ()) = function
    | `V1 (resp, body) ->
        let rec on_eof = shutdown
        and on_read bstr ~off ~len =
          let str = Bigstringaf.substring bstr ~off ~len in
          acc := f !acc str;
          Httpaf.Body.schedule_read body ~on_read ~on_eof
        in
        response := Some (`V1 resp);
        Httpaf.Body.schedule_read body ~on_read ~on_eof
    | `V2 (resp, body) ->
        let rec on_eof () =
          Log.debug (fun m ->
              m "shutdown the connection from the application level");
          shutdown ()
        and on_read bstr ~off ~len =
          let str = Bigstringaf.substring bstr ~off ~len in
          acc := f !acc str;
          H2.Body.Reader.schedule_read body ~on_read ~on_eof
        in
        response := Some (`V2 resp);
        H2.Body.Reader.schedule_read body ~on_read ~on_eof
  in
  let give =
    match flow with
    | `Tls flow -> [ Miou_unix.owner flow.TLS.flow ]
    | `Tcp flow -> [ Miou_unix.owner flow ]
  in
  match (flow, config, request) with
  | `Tls flow, `V1 config, `V1 request ->
      let read_buffer_size = config.Httpaf.Config.read_buffer_size in
      let disown flow = Miou_unix.disown flow.TLS.flow in
      let response_handler resp body = response_handler (`V1 (resp, body)) in
      let error_handler error = error_handler (`V1 error) in
      let body, conn =
        Httpaf.Client_connection.request ~config request ~error_handler
          ~response_handler
      in
      let orphans = Miou.orphans () in
      let { A.protect }, prm =
        disown flow;
        Log.debug (fun m -> m "start an http/1.1 request over TLS");
        A.run conn ~give ~disown ~read_buffer_size flow
      in
      let await () =
        match (Miou.await prm, !error, !response) with
        | _, Some error, _ -> Error error
        | Error exn, _, _ -> Error (`V1 (`Exn exn))
        | Ok (), None, Some (`V1 response) -> Ok (response, !acc)
        | Ok (), None, (Some (`V2 _) | None) -> assert false
      in
      let write_string body ?off ?len str =
        protect ~orphans (Httpaf.Body.write_string body ?off ?len) str
      in
      let close body = protect ~orphans Httpaf.Body.close_writer body in
      let body = { body; write_string; close } in
      (orphans, Process (V1, await, body))
  | `Tcp flow, `V1 config, `V1 request ->
      let read_buffer_size = config.Httpaf.Config.read_buffer_size in
      let disown = Miou_unix.disown in
      let response_handler resp body = response_handler (`V1 (resp, body)) in
      let error_handler error = error_handler (`V1 error) in
      let body, conn =
        Httpaf.Client_connection.request ~config request ~error_handler
          ~response_handler
      in
      let orphans = Miou.orphans () in
      let { B.protect }, prm =
        disown flow;
        B.run conn ~give ~disown ~read_buffer_size flow
      in
      let await () =
        match (Miou.await prm, !error, !response) with
        | _, Some error, _ -> Error error
        | Error exn, _, _ -> Error (`V1 (`Exn exn))
        | Ok (), None, Some (`V1 response) -> Ok (response, !acc)
        | Ok (), None, (Some (`V2 _) | None) -> assert false
      in
      let write_string body ?off ?len str =
        protect ~orphans (Httpaf.Body.write_string body ?off ?len) str
      in
      let close body = protect ~orphans Httpaf.Body.close_writer body in
      let body = { body; write_string; close } in
      (orphans, Process (V1, await, body))
  | `Tls flow, `V2 config, `V2 request ->
      let read_buffer_size = config.H2.Config.read_buffer_size in
      let disown flow = Miou_unix.disown flow.TLS.flow in
      let error_handler error = error_handler (`V2 error) in
      let conn = H2.Client_connection.create ~config ~error_handler () in
      let shutdown () = H2.Client_connection.shutdown conn in
      let response_handler resp body =
        response_handler ~shutdown (`V2 (resp, body))
      in
      let body =
        H2.Client_connection.request conn request ~error_handler
          ~response_handler
      in
      let orphans = Miou.orphans () in
      let { C.protect }, prm =
        disown flow;
        Log.debug (fun m -> m "start an h2 request over TLS");
        C.run conn ~give ~disown ~read_buffer_size flow
      in
      let await () =
        match (Miou.await prm, !error, !response) with
        | _, Some error, _ -> Error error
        | Error exn, _, _ -> Error (`V2 (`Exn exn))
        | Ok (), None, Some (`V2 response) -> Ok (response, !acc)
        | Ok (), None, (Some (`V1 _) | None) -> assert false
      in
      let write_string body ?off ?len str =
        protect ~orphans (H2.Body.Writer.write_string body ?off ?len) str
      in
      let close body =
        Log.debug (fun m -> m "close the stream from the application level");
        protect ~orphans H2.Body.Writer.close body
      in
      let body = { body; write_string; close } in
      (orphans, Process (V2, await, body))
  | `Tcp flow, `V2 config, `V2 request ->
      let read_buffer_size = config.H2.Config.read_buffer_size in
      let disown = Miou_unix.disown in
      let error_handler error = error_handler (`V2 error) in
      let conn = H2.Client_connection.create ~config ~error_handler () in
      let shutdown () = H2.Client_connection.shutdown conn in
      let response_handler resp body =
        response_handler ~shutdown (`V2 (resp, body))
      in
      let body =
        H2.Client_connection.request conn request ~error_handler
          ~response_handler
      in
      let orphans = Miou.orphans () in
      let { D.protect }, prm =
        disown flow;
        D.run conn ~give ~disown ~read_buffer_size flow
      in
      let await () =
        match (Miou.await prm, !error, !response) with
        | _, Some error, _ -> Error error
        | Error exn, _, _ -> Error (`V2 (`Exn exn))
        | Ok (), None, Some (`V2 response) -> Ok (response, !acc)
        | Ok (), None, (Some (`V1 _) | None) -> assert false
      in
      let write_string body ?off ?len str =
        protect ~orphans (H2.Body.Writer.write_string body ?off ?len) str
      in
      let close body = protect ~orphans H2.Body.Writer.close body in
      let body = { body; write_string; close } in
      (orphans, Process (V2, await, body))
  | _ -> Fmt.invalid_arg "Http_miou_unix.run: incompatible arguments"
