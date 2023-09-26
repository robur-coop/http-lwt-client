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
    | Ok `End_of_input -> report_closed ()
    | Error err -> report_error err
    | _ -> ()

  let close flow =
    if flow.reader_closed && flow.writer_closed then Flow.close flow.flow

  let shutdown v flow =
    match v with
    | `Recv ->
        flow.reader_closed <- true;
        close flow
    | `Send ->
        flow.writer_closed <- true;
        close flow

  let blit src src_off dst dst_off len =
    let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
    Cstruct.blit src src_off dst 0 len

  let read flow ~report_error ~report_closed ~read ~read_eof =
    Ke.Rke.compress flow.queue;
    match Flow.read flow.flow with
    | (Error _ | Ok `End_of_input) as v ->
        shutdown `Recv flow;
        let _shift =
          match Ke.Rke.N.peek flow.queue with
          | [] -> read_eof Bigstringaf.empty ~off:0 ~len:0
          | [ slice ] -> read_eof slice ~off:0 ~len:(Bigstringaf.length slice)
          | _ -> assert false
        in
        report ~report_error ~report_closed v;
        `Closed
    | Ok (`Data ({ Cstruct.len; _ } as v)) ->
        Ke.Rke.N.push flow.queue ~blit ~length:Cstruct.length ~off:0 ~len v;
        let[@warning "-8"] (slice :: _) = Ke.Rke.N.peek flow.queue in
        let shift = read slice ~off:0 ~len:(Bigstringaf.length slice) in
        Ke.Rke.N.shift_exn flow.queue shift;
        `Continue

  let write ~report_error flow iovecs =
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
        report_error err;
        shutdown `Send flow;
        `Closed

  let write ~report_error flow iovecs =
    if flow.writer_closed then begin
      close flow;
      `Closed
    end
    else write ~report_error flow iovecs

  let close flow =
    match (flow.reader_closed, flow.writer_closed) with
    | true, true -> ()
    | _ ->
        flow.reader_closed <- true;
        flow.writer_closed <- true;
        Flow.close flow.flow
end

let src = Logs.Src.create "http-client-unix"

module Log = (val Logs.src_log src : Logs.LOG)

let catch ~on fn = try fn () with exn -> on exn

let rec clean orphans =
  match Miou.care orphans with
  | Some prm ->
      Miou.await_exn prm;
      clean orphans
  | None -> ()

exception Flow of string

module type S = sig
  type conn
  type flow

  val run :
    conn ->
    ?give:Miou.Ownership.t list ->
    ?disown:(flow -> unit) ->
    flow ->
    unit
end

module Make (Flow : Flow.S) (Runtime : RUNTIME) :
  S with type conn = Runtime.t and type flow = Flow.t = struct
  type conn = Runtime.t
  type flow = Flow.t

  module Buffered_flow = Buffered_flow (Flow)

  let to_flow_exception err : exn = Flow (Fmt.str "%a" Flow.pp_error err)

  let run connection ?give ?(disown = Fun.const ()) flow =
    let flow = Buffered_flow.create flow in
    let orphans = Miou.orphans () in
    let notify_reader_exit = ref false in
    let notify_writer_exit = ref false in

    let rec reader () =
      let report_error err =
        Runtime.report_exn connection (to_flow_exception err)
      in
      let rec go () =
        match Runtime.next_read_operation connection with
        | `Read ->
            Log.debug (fun m -> m "next read operation: `read");
            let _ =
              Buffered_flow.read flow ~report_error ~report_closed:ignore
                ~read:(Runtime.read connection)
                ~read_eof:(Runtime.read_eof connection)
            in
            go ()
        | `Yield ->
            Log.debug (fun m -> m "next read operation: `yield");
            let continuation () = ignore (Miou.call_cc ~orphans ?give reader) in
            Runtime.yield_reader connection continuation;
            Miou.yield ()
        | `Close ->
            Log.debug (fun m -> m "next read operation: `close.");
            Buffered_flow.shutdown `Recv flow;
            notify_reader_exit := true
      in
      Fun.protect ~finally:(fun () -> disown flow.Buffered_flow.flow)
      @@ fun () -> catch ~on:(Runtime.report_exn connection) go
    in
    let rec writer () =
      let report_error err =
        Runtime.report_exn connection (to_flow_exception err)
      in
      let rec go () =
        match Runtime.next_write_operation connection with
        | `Write iovecs ->
            Log.debug (fun m -> m "next write operation: `write.");
            let result = Buffered_flow.write ~report_error flow iovecs in
            Runtime.report_write_result connection result;
            go ()
        | `Yield ->
            Log.debug (fun m -> m "next write operation: `yield.");
            let continuation () = ignore (Miou.call_cc ~orphans ?give writer) in
            Runtime.yield_writer connection continuation;
            Miou.yield ()
        | `Close _ ->
            Log.debug (fun m -> m "next write operation: `close.");
            Buffered_flow.shutdown `Send flow;
            notify_writer_exit := true
      in
      Fun.protect ~finally:(fun () -> disown flow.Buffered_flow.flow)
      @@ fun () -> catch ~on:(Runtime.report_exn connection) go
    in
    let _ = Miou.call_cc ~orphans ?give reader in
    let _ = Miou.call_cc ~orphans ?give writer in
    let rec go () =
      clean orphans;
      match (!notify_reader_exit, !notify_writer_exit) with
      | true, true -> clean orphans
      | _ ->
          Miou.yield ();
          go ()
    in
    go ();
    Log.debug (fun m -> m "End of transmission.");
    let prm = Miou.call_cc ?give @@ fun () -> Buffered_flow.close flow in
    clean orphans;
    Miou.await_exn prm
end

module TCP = struct
  type t = Miou_unix.file_descr
  type error = Unix.error * string * string

  let pp_error ppf (err, f, v) =
    Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)

  let read flow =
    let buf = Bytes.create 0x1000 in
    match Miou_unix.read flow buf ~off:0 ~len:(Bytes.length buf) with
    | 0 -> Ok `End_of_input
    | len -> Ok (`Data (Cstruct.of_string (Bytes.sub_string buf 0 len)))
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

type ('resp, 'body) version =
  | V1 : (Httpaf.Response.t, [ `write ] Httpaf.Body.t) version
  | V2 : (H2.Response.t, H2.Body.Writer.t) version

type error =
  [ `V1 of Httpaf.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string ]

type 'acc process =
  | Process :
      ('resp, 'body) version * ('resp * 'acc, error) result Miou.t * 'body
      -> 'acc process

module HTTPS_1_1 = Make (TLS) (Httpaf_Client_connection)
module HTTP_1_1 = Make (TCP) (Httpaf_Client_connection)
module RH2S = Make (TLS) (H2.Client_connection)
module RH2 = Make (TCP) (H2.Client_connection)

(* NOTE(dinosaure): we avoid first-class module here. *)
let run ~f acc config flow request =
  let response : response option ref = ref None
  and error = ref None
  and acc = ref acc in
  let error_handler = function
    | `V1 (`Exn (Flow msg)) | `V2 (`Exn (Flow msg)) ->
        error := Some (`Protocol msg)
    | err -> error := Some err
  in
  let response_handler = function
    | `V1 (resp, body) ->
        let rec on_eof () = ()
        and on_read bstr ~off ~len =
          let str = Bigstringaf.substring bstr ~off ~len in
          acc := f !acc str;
          Httpaf.Body.schedule_read body ~on_read ~on_eof
        in
        response := Some (`V1 resp);
        Httpaf.Body.schedule_read body ~on_read ~on_eof
    | `V2 (_response, _body) -> assert false
  in
  match (flow, config, request) with
  | `Tls flow, `V1 config, `V1 request ->
      let give = [ Miou_unix.owner flow.TLS.flow ] in
      let disown flow = Miou_unix.disown flow.TLS.flow in
      let response_handler resp body = response_handler (`V1 (resp, body)) in
      let error_handler error = error_handler (`V1 error) in
      let body, conn =
        Httpaf.Client_connection.request ~config request ~error_handler
          ~response_handler
      in
      let prm =
        Miou.call_cc ~give @@ fun () ->
        HTTPS_1_1.run conn ~give ~disown flow;
        Miou.yield ();
        match (!error, !response) with
        | Some error, _ -> Error error
        | None, Some (`V1 response) -> Ok (response, !acc)
        | _ -> assert false
      in
      Process (V1, prm, body)
  | `Tcp flow, `V1 config, `V1 request ->
      let give = [ Miou_unix.owner flow ] in
      let disown = Miou_unix.disown in
      let response_handler resp body = response_handler (`V1 (resp, body)) in
      let error_handler err = error_handler (`V1 err) in
      let body, conn =
        Httpaf.Client_connection.request ~config request ~error_handler
          ~response_handler
      in
      let prm =
        Miou.call_cc ~give @@ fun () ->
        HTTP_1_1.run conn ~give ~disown flow;
        Miou.yield ();
        match (!error, !response) with
        | Some error, _ -> Error error
        | None, Some (`V1 response) -> Ok (response, !acc)
        | _ -> assert false
      in
      Process (V1, prm, body)
  | `Tls flow, `V2 config, `V2 request ->
      let give = [ Miou_unix.owner flow.TLS.flow ] in
      let disown flow = Miou_unix.disown flow.TLS.flow in
      let response_handler resp body = response_handler (`V2 (resp, body)) in
      let error_handler err = error_handler (`V2 err) in
      let conn = H2.Client_connection.create ~config ~error_handler () in
      let body =
        H2.Client_connection.request conn request ~error_handler
          ~response_handler
      in
      let prm =
        Miou.call_cc ~give @@ fun () ->
        RH2S.run conn ~give ~disown flow;
        Miou.yield ();
        match (!error, !response) with
        | Some error, _ -> Error error
        | None, Some (`V2 response) -> Ok (response, !acc)
        | _ -> assert false
      in
      Process (V2, prm, body)
  | `Tcp flow, `V2 config, `V2 request ->
      let give = [ Miou_unix.owner flow ] in
      let disown = Miou_unix.disown in
      let response_handler resp body = response_handler (`V2 (resp, body)) in
      let error_handler err = error_handler (`V2 err) in
      let conn = H2.Client_connection.create ~config ~error_handler () in
      let body =
        H2.Client_connection.request conn request ~error_handler
          ~response_handler
      in
      let prm =
        Miou.call_cc ~give @@ fun () ->
        RH2.run conn ~give ~disown flow;
        Miou.yield ();
        match (!error, !response) with
        | Some error, _ -> Error error
        | None, Some (`V2 response) -> Ok (response, !acc)
        | _ -> assert false
      in
      Process (V2, prm, body)
  | _ -> Fmt.invalid_arg "Http_miou_unix.run: incompatible arguments"
