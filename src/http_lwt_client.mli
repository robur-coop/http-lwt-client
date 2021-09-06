type response =
  | HTTP_1_1 of Httpaf.Response.t
  | H2 of H2.Response.t

val one_request
  : ?config : [ `HTTP_1_1 of Httpaf.Config.t | `H2 of H2.Config.t ]
  -> ?authenticator:X509.Authenticator.t
  -> ?meth:Httpaf.Method.t
  -> ?headers:(string * string) list
  -> ?body:string
  -> ?max_redirect:int
  -> string
  -> (response * string option, [> `Msg of string ]) Lwt_result.t
