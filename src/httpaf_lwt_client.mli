val one_request
  : ?config : Httpaf.Config.t
  -> ?authenticator:X509.Authenticator.t
  -> ?meth:Httpaf.Method.t
  -> ?headers:Httpaf.Headers.t
  -> ?body:string
  -> ?max_redirect:int
  -> string
  -> (Httpaf.Response.t * string option, [> `Msg of string ]) Lwt_result.t
