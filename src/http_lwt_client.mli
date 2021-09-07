module Version = Httpaf.Version

module Status = H2.Status

module Headers = H2.Headers

type response =
  { version : Version.t
  ; status  : Status.t
  ; reason  : string
  ; headers : Headers.t }

val pp_response : Format.formatter -> response -> unit

val one_request
  : ?config : [ `HTTP_1_1 of Httpaf.Config.t | `H2 of H2.Config.t ]
  -> ?authenticator:X509.Authenticator.t
  -> ?meth:Httpaf.Method.t
  -> ?headers:(string * string) list
  -> ?body:string
  -> ?max_redirect:int
  -> ?follow_redirect:bool
  -> string
  -> (response * string option, [> `Msg of string ]) Lwt_result.t
