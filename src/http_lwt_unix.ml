(*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.
    Copyright (c) 2018 Anton Bachin

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Lwt.Infix

let src = Logs.Src.create "http_lwt_unix" ~doc:"HTTP client, unix"
module Log = (val Logs.src_log src : Logs.LOG)

module Buffer : sig
  type t

  val create : int -> t

  val get : t -> f:(Lwt_bytes.t -> off:int -> len:int -> int) -> int
  val put : t -> f:(Lwt_bytes.t -> off:int -> len:int -> int Lwt.t) -> int Lwt.t
end = struct
  type t =
    { mutable buffer : Lwt_bytes.t
    ; mutable off : int
    ; mutable len : int }

  let create size =
    let buffer = Lwt_bytes.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0
    then begin
      t.off <- 0;
      t.len <- 0;
    end else if t.off > 0
    then begin
      Lwt_bytes.blit t.buffer t.off t.buffer 0 t.len;
      t.off <- 0;
    end

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0
    then t.off <- 0;
    n

  let put t ~f =
    compress t;
    let off = t.off + t.len in
    let buf = t.buffer in
    if Lwt_bytes.length buf = t.len then begin
      t.buffer <- Lwt_bytes.create (2 * Lwt_bytes.length buf);
      Lwt_bytes.blit buf t.off t.buffer 0 t.len;
    end;
    f t.buffer ~off ~len:(Lwt_bytes.length t.buffer - off)
    >>= fun n ->
    t.len <- t.len + n;
    Lwt.return n
end

type connection = [ `Plain of Lwt_unix.file_descr | `Tls of Tls_lwt.Unix.t ]
type keep_alive = { mutable fd : connection option
                  ; lock : Lwt_mutex.t
                  ; mutable max : int }

let new_keep_alive () = { fd = None; lock = Lwt_mutex.create (); max = 100 }

let keep_alive_lock = function
  | Some _r -> Lwt.return_unit (*Lwt_mutex.lock r.lock*)
  | None -> Lwt.return_unit

let keep_alive_unlock = function
  | Some ke -> if Lwt_mutex.is_locked ke.lock then Lwt_mutex.unlock ke.lock
  | None -> ()

let reset_keep_alive ?(close=false) keep_alive =
  match keep_alive with
  | Some ke ->
     if Lwt_mutex.is_locked ke.lock then Lwt_mutex.unlock ke.lock;
     Lwt.catch
       (fun () -> let fd = ke.fd in
                  ke.fd <- None;
                  if close then
                    match fd with
                    | Some (`Plain fd) ->
                       if Lwt_unix.state fd <> Lwt_unix.Closed then Lwt_unix.close fd
                       else Lwt.return_unit
                    | Some (`Tls t) -> Tls_lwt.Unix.close t
                    | None -> Lwt.return_unit
                  else Lwt.return_unit) (* ignore if close fails *)
       (fun _ -> Lwt.return_unit)
  | _ -> Lwt.return_unit

let must_close keep_alive =
  match keep_alive with
  | None -> true
  | Some { max; _ } when max <= 1 -> true
  | _ -> false

let close keep_alive fd =
  (match fd with
     | `Plain fd ->
        if Lwt_unix.state fd <> Lwt_unix.Closed then Lwt_unix.close fd
        else Lwt.return_unit
     | `Tls t -> Tls_lwt.Unix.close t)
  >>= (fun () -> reset_keep_alive keep_alive)

let read keep_alive fd buffer =
  Lwt.catch
    (fun () ->
      Buffer.put buffer ~f:(fun bigstring ~off ~len ->
          match fd with
          | `Plain fd -> Lwt_bytes.read fd bigstring off len
          | `Tls t -> Tls_lwt.Unix.read_bytes t bigstring off len))
    (function
    | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
       Log.err (fun m -> m "bad fd in read");
       reset_keep_alive keep_alive >>= (fun () -> Lwt.fail exn)
    | exn ->
      Log.err (fun m -> m "exception read %s" (Printexc.to_string exn));
      close keep_alive fd >>= fun () ->
      Lwt.fail exn)

  >>= fun bytes_read ->
  if bytes_read = 0 then
    Lwt.return `Eof
  else
    Lwt.return (`Ok bytes_read)

let shutdown socket command =
  try Lwt_unix.shutdown socket command
  with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

module type RUNTIME = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  val yield_reader : t -> (unit -> unit) -> unit
  val next_write_operation : t -> [ `Write of Bigstringaf.t Faraday.iovec list | `Close of int | `Yield ]
  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val yield_writer : t -> (unit -> unit) -> unit
  val report_exn : t -> exn -> unit
end

module Make (Runtime : RUNTIME) = struct
  let request ?(read_buffer_size = 0x1000) ?keep_alive socket connection =

    let module Client_connection = Httpaf.Client_connection in

    let read_buffer = Buffer.create read_buffer_size in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let rec read_loop () =
      let rec read_loop_step () =
        match Runtime.next_read_operation connection with
        | `Yield ->
           Runtime.yield_reader connection read_loop;
           Lwt.return_unit
        | `Read ->
             read keep_alive socket read_buffer >>= begin
              function
              | `Eof ->
                 Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
                       Runtime.read_eof connection bigstring ~off ~len)
                 |> ignore;
                 read_loop_step ()
              | `Ok _ ->
                 Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
                     Runtime.read connection bigstring ~off ~len)
                 |> ignore;
                 Lwt.return_unit >>= read_loop_step
            end

        | `Close ->
           Lwt.wakeup_later notify_read_loop_exited ();
           if must_close keep_alive then
             begin
               match socket with
               | `Plain socket ->
                  if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
                      shutdown socket Unix.SHUTDOWN_RECEIVE
                    end;
               | `Tls _t -> () (* no shutdown of receive part in TLS *)
             end
           else keep_alive_unlock keep_alive;
           Lwt.return_unit

      in

      Lwt.async (fun () ->
          Lwt.catch
            read_loop_step
            (fun exn ->
              Runtime.report_exn connection exn;
              Lwt.return_unit))
    in


    let writev =
      match socket with
      | `Plain socket -> Faraday_lwt_unix.writev_of_fd socket
      | `Tls t ->
         fun vs ->
           let cs =
             List.map (fun { Faraday.buffer ; off ; len } ->
                 Cstruct.of_bigarray ~off ~len buffer) vs
           in
           Lwt.catch (fun () ->
               Tls_lwt.Unix.writev t cs >|= fun () ->
               `Ok (Cstruct.lenv cs))
             (fun exn ->
               Log.err (fun m -> m "exception writev: %s" (Printexc.to_string exn));
               close keep_alive socket
               >|= fun () -> `Closed)
      in
      let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

      let rec write_loop () =
        let rec write_loop_step () =
          match Runtime.next_write_operation connection with
          | `Write io_vectors ->
             writev io_vectors >>= fun result ->
             Runtime.report_write_result connection result;
             Lwt.return_unit >>= write_loop_step

          | `Yield ->
             Runtime.yield_writer connection write_loop;
             Lwt.return_unit

          | `Close _ ->
             Lwt.wakeup_later notify_write_loop_exited ();
             Lwt.return_unit
        in

        Lwt.async (fun () ->
            Lwt.catch
              write_loop_step
              (fun exn ->
                Runtime.report_exn connection exn;
                Lwt.return_unit))
      in


      read_loop ();
      write_loop ();
      Lwt.async (fun () ->
          Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->
          if must_close keep_alive then close keep_alive socket
          else Lwt.return_unit)

end


module Httpaf_client_connection = struct
  include Httpaf.Client_connection
  let yield_reader _ = assert false
  let next_read_operation connection =
    (next_read_operation connection :> [ `Close | `Read | `Yield ])
end

module Client_HTTP_1_1 = Make (Httpaf_client_connection)
module Client_H2 = Make (H2.Client_connection)
