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

let src = Logs.Src.create "buffered-flow"

module Logb = (val Logs.src_log src : Logs.LOG)

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

(*
module Buffered_flow (Flow : Flow.S) = struct
  type t = {
    flow : Flow.t;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    mutable reader_closed : bool;
    mutable writer_closed : bool;
  }

  let create ?(capacity = 0x1000) flow =
    let queue = Ke.Rke.create ~capacity Bigarray.char in
    { flow; queue; reader_closed = false; writer_closed = false }

  let report ~report_error ~report_closed = function
    | Ok `End_of_input ->
        Logb.debug (fun m -> m "the connection was closed by peer");
        report_closed ()
    | Error err ->
        Logb.err (fun m ->
            m "got an error while reading the file-descriptor: %a" Flow.pp_error
              err);
        report_error err
    | _ -> ()

  let shutdown ~disown v t =
    match (v, t.reader_closed, t.writer_closed) with
    | `Recv, true, _ -> disown t.flow
    | `Send, _, true -> disown t.flow
    | `Recv, false, _ ->
        Logb.debug (fun m ->
            m "shutdown (recv) the file-descriptor (rd: %b, wr: %b)"
              t.reader_closed t.writer_closed);
        t.reader_closed <- true;
        if t.writer_closed then Flow.close t.flow
        else Flow.shutdown t.flow `Recv
    | `Send, _, false ->
        Logb.debug (fun m ->
            m "shutdown (send) the file-descriptor (rd: %b, wr: %b)"
              t.reader_closed t.writer_closed);
        t.writer_closed <- true;
        Flow.shutdown t.flow `Send

  let blit src src_off dst dst_off len =
    let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
    Cstruct.blit src src_off dst 0 len

  let read flow ~read_buffer_size ~report_error ~read ~read_eof =
    Ke.Rke.compress flow.queue;
    match Flow.read ~read_buffer_size flow.flow with
    | (Error _ | Ok `End_of_input) as v ->
        let shift =
          match Ke.Rke.N.peek flow.queue with
          | slice :: _ ->
              let len = min read_buffer_size (Bigstringaf.length slice) in
              read slice ~off:0 ~len
          | [] -> read_eof Bigstringaf.empty ~off:0 ~len:0
        in
        Ke.Rke.N.shift_exn flow.queue shift;
        report ~report_error ~report_closed:ignore v;
        `Closed
    | Ok (`Data ({ Cstruct.len; _ } as v)) ->
        Logb.debug (fun m -> m "got %d byte(s)" len);
        Logb.debug (fun m ->
            m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) (Cstruct.to_string v));
        Ke.Rke.N.push flow.queue ~blit ~length:Cstruct.length ~off:0 ~len v;
        let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek flow.queue in
        let len = min read_buffer_size (Bigstringaf.length slice) in
        Logb.debug (fun m ->
            m "transfer %d byte(s) into the runtime (max: %d)" len
              read_buffer_size);
        let shift = read slice ~off:0 ~len in
        Logb.debug (fun m -> m "shift %d byte(s)" shift);
        Ke.Rke.N.shift_exn flow.queue shift;
        `Continue

  let write ~disown ~report_error flow iovecs =
    let iovecs =
      List.map
        (fun { Faraday.buffer; off; len } ->
          Cstruct.to_string (Cstruct.of_bigarray buffer ~off ~len) ~off:0 ~len)
        iovecs
    in
    let iovecs = List.map Cstruct.of_string iovecs in
    match Flow.writev flow.flow iovecs with
    | Ok () ->
        `Ok (List.fold_left (fun a { Cstruct.len; _ } -> a + len) 0 iovecs)
    | Error err ->
        Logb.err (fun m ->
            m "got an error when we tried to write something: %a" Flow.pp_error
              err);
        report_error err;
        shutdown ~disown `Send flow;
        `Closed

  let write ~disown ~report_error flow iovecs =
    if flow.writer_closed then begin
      Logb.err (fun m -> m "the file-descriptor is already closed");
      shutdown ~disown `Send flow;
      `Closed
    end
    else write ~disown ~report_error flow iovecs

  let close ~disown flow =
    match (flow.reader_closed, flow.writer_closed) with
    | true, true -> disown flow.flow
    | _ ->
        Logb.debug (fun m -> m "really close the file-descriptor");
        flow.reader_closed <- true;
        flow.writer_closed <- true;
        Flow.close flow.flow
end
*)

let src = Logs.Src.create "http-miou-unix"

module Logr = (val Logs.src_log src : Logs.LOG)

let catch ~on fn =
  try fn ()
  with exn ->
    Logr.err (fun m ->
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
    | Error _err ->
        Flow.close flow;
        `Closed

  type _ Effect.t += Writer : unit Effect.t
  type _ Effect.t += Reader : unit Effect.t

  let launch_writer ~orphans ~give ~writer k =
    let _prm = Miou.call_cc ~orphans ~give writer in
    Effect.Deep.continue k ()

  let launch_reader ~orphans ~give ~reader k =
    let _prm = Miou.call_cc ~orphans ~give reader in
    Effect.Deep.continue k ()

  (* Protected runtime operations. *)

  let protect ~orphans ~give ~reader ~writer fn v =
    let retc = Fun.id in
    let exnc = raise in
    let open Effect.Deep in
    let effc : type c. c Effect.t -> ((c, 'b) continuation -> 'b) option =
      function
      | Writer -> Some (launch_writer ~orphans ~give ~writer)
      | Reader -> Some (launch_reader ~orphans ~give ~reader)
      | _ -> None
    in
    match_with fn v { retc; exnc; effc }

  let next_read_operation ~orphans ~give ~reader ~writer =
    protect ~orphans ~give ~reader ~writer Runtime.next_read_operation

  let next_write_operation ~orphans ~give ~reader ~writer =
    protect ~orphans ~give ~reader ~writer Runtime.next_write_operation

  let read ~orphans ~give ~reader ~writer conn bstr ~off ~len =
    protect ~orphans ~give ~reader ~writer (Runtime.read conn ~off ~len) bstr

  let read_eof ~orphans ~give ~reader ~writer conn bstr ~off ~len =
    protect ~orphans ~give ~reader ~writer
      (Runtime.read_eof conn ~off ~len)
      bstr

  let report_exn ~orphans ~give ~reader ~writer conn exn =
    Logr.err (fun m -> m "report an exception: %S" (Printexc.to_string exn));
    protect ~orphans ~give ~reader ~writer (Runtime.report_exn conn) exn

  let report_write_result ~orphans ~give ~reader ~writer conn =
    protect ~orphans ~give ~reader ~writer (Runtime.report_write_result conn)

  let yield_reader ~orphans ~give ~reader ~writer conn =
    protect ~orphans ~give ~reader ~writer (Runtime.yield_reader conn)

  let yield_writer ~orphans ~give ~reader ~writer conn =
    protect ~orphans ~give ~reader ~writer (Runtime.yield_writer conn)

  type protect = {
    protect : 'a 'b. orphans:unit Miou.orphans -> ('a -> 'b) -> 'a -> 'b;
  }
  [@@unboxed]

  let run conn ?(give = []) ?(disown = Fun.const ()) ~read_buffer_size flow =
    let buffer = Buffer.create read_buffer_size in

    let rec reader () =
      let rec go orphans () =
        match next_read_operation ~orphans ~give ~reader ~writer conn with
        | `Read -> (
            Logr.debug (fun m -> m "next read operation: `read");
            let read_eof = read_eof ~orphans ~give ~reader ~writer in
            let read = read ~orphans ~give ~reader ~writer in
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
            Logr.debug (fun m -> m "next read operation: `yield");
            let continuation () = Effect.perform Reader in
            yield_reader conn ~orphans ~give ~reader ~writer continuation;
            disown flow;
            terminate orphans
        | `Close ->
            Logr.debug (fun m -> m "read: disown the file-descriptor");
            disown flow;
            terminate orphans
      in
      let orphans = Miou.orphans () in
      catch ~on:(report_exn conn ~orphans ~give ~reader ~writer) @@ fun () ->
      go orphans ()
    and writer () =
      let rec go orphans () =
        match next_write_operation ~orphans ~give ~reader ~writer conn with
        | `Write iovecs ->
            Logr.debug (fun m -> m "next write operation: `write");
            writev flow iovecs
            |> report_write_result conn ~orphans ~give ~reader ~writer;
            go orphans ()
        | `Yield ->
            Logr.debug (fun m -> m "next write operation: `yield");
            let continuation () = Effect.perform Writer in
            yield_writer conn ~orphans ~give ~reader ~writer continuation;
            disown flow;
            terminate orphans
        | `Close _ ->
            Logr.debug (fun m -> m "next write operation: `close");
            Flow.shutdown flow `Send;
            terminate orphans
      in
      let orphans = Miou.orphans () in
      catch ~on:(report_exn conn ~orphans ~give ~reader ~writer) @@ fun () ->
      go orphans ()
    in
    let protect = protect ~give ~reader ~writer in
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
      Logr.debug (fun m -> m "close the file-descriptor");
      disown flow;
      match result with Ok () -> () | Error exn -> raise exn
    in
    Logr.debug (fun m -> m "the main task is: %a" Miou.Promise.pp prm);
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
    Logr.err (fun m -> m "Got an error: %a" pp_error err);
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
          Logr.debug (fun m ->
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
        Logr.debug (fun m -> m "start an http/1.1 request over TLS");
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
        Logr.debug (fun m -> m "start an h2 request over TLS");
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
        Logr.debug (fun m -> m "close the stream from the application level");
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
