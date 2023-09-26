type tls
type tls_error

val pp_tls_error : tls_error Fmt.t

val to_tls :
  Tls.Config.client ->
  ?host:[ `host ] Domain_name.t ->
  Miou_unix.file_descr ->
  (tls, tls_error) result

val epoch : tls -> Tls.Core.epoch_data option

type config = [ `V1 of Httpaf.Config.t | `V2 of H2.Config.t ]
type flow = [ `Tls of tls | `Tcp of Miou_unix.file_descr ]
type request = [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ]

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

val run :
  f:('acc -> string -> 'acc) ->
  'acc ->
  config ->
  flow ->
  request ->
  'acc process
