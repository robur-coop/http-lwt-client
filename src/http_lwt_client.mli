(** HTTP lwt client

    A HTTP client using the lwt task library. It does a single HTTP request
    (though may follow redirects) to a remote uri. Both HTTP protocol 1.1 and
    2.0 are supported. Both http and https (via the pure implementation
    OCaml-TLS) are supported. A connection is established via the
    happy-eyeballs algorithm. *)

(** Protocol Version

    Consists of major.minor, in HTTP2 this is 2.0.
*)
module Version = Httpaf.Version

(** Response Status Codes

    A three-digit integer, the result of the request. *)
module Status = H2.Status

(** Header fields

    Case-insensitive key-value pairs. *)
module Headers = H2.Headers

(** A response, consisting of version, status, reason (HTTP 1 only), and
    headers. *)
type response =
  { version : Version.t
  ; status  : Status.t
  ; reason  : string
  ; headers : Headers.t }

(** [pp_response ppf response] pretty-prints the [response] on [ppf]. *)
val pp_response : Format.formatter -> response -> unit

type keep_alive

val new_keep_alive : unit -> keep_alive
val active_keep_alive : keep_alive option -> bool
val reset_keep_alive : ?close:bool -> keep_alive option -> unit Lwt.t

(** [request ~config ~authenticator ~meth ~headers ~body ~max_redirect
    ~follow_redirect ~happy_eyeballs uri f init] does a single request of [uri]
    and returns the response. Each time part of the body is received,
    [f acc part] is called, with [acc] being the last return value of [f]
    (or [init] if it is the first time) and [part] being the body part received.

    By default, up to 5 redirects ([max_redirect]) are followed.
    If [follow_redirect] is false, no redirect is followed (defaults to true).
    The default HTTP request type ([meth]) is [GET].

    If no [tls_config] is provided, a default one is used, with alpn and
    authenticators. If a [tls_config] is provided, this is used unmodified.

    If no [config] is provided (the default), and the [uri] uses the [https]
    schema, application layer next protocol negotiation (ALPN) is used to
    negotiate HTTP2 (prefered) or HTTP1. If the [uri] uses the [http] schema,
    HTTP1 is used by default (unless the [config] provided is [`H2]).

    The default [authenticator] is from the [ca-certs] opam package, which
    discovers and uses the system trust anchors.

    The [happy-eyeballs] opam package is used to establish the TCP connection,
    which prefers IPv6 over IPv4.
*)
val request
  : ?config : [ `HTTP_1_1 of Httpaf.Config.t | `H2 of H2.Config.t ]
  -> ?tls_config:Tls.Config.client
  -> ?authenticator:X509.Authenticator.t
  -> ?meth:Httpaf.Method.t
  -> ?headers:(string * string) list
  -> ?body:string
  -> ?max_redirect:int
  -> ?follow_redirect:bool
  -> ?happy_eyeballs:Happy_eyeballs_lwt.t
  -> ?keep_alive:keep_alive
  -> string
  -> (response -> 'a -> string -> 'a Lwt.t)
  -> 'a
  -> (response * 'a, [ `Msg of string ]) Lwt_result.t
