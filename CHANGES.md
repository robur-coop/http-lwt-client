## v0.0.7 (2022-08-25)

* upgrade to h2 0.9.0 API (#10 @hannesm)

## v0.0.6 (2022-03-16)

* upgrade to cmdliner 1.1.0 API (@hannesm)

## v0.0.5 (2021-10-30)

* remove rresult dependency (#6 @hannesm @reynir)

## v0.0.4 (2021-09-11)

* Add ?tls_config to Http_lwt_client.one_request to enable passing an entire
  TLS configuration
* Avoid exception in resolve_location
* Add ?happy_eyeballs to Http_lwt_client.one_request to allow reusing this
  state across requests
* Initialize Ca_certs.authenticator only once (lazily) to avoid decoding
  trust anchors multiple times
* Fix Http_lwt_unix.Buffer for resizing on put (to avoid errors with H2)
* Avoid exceptions from Lwt.wakeup_later by calling it at most once

## v0.0.3 (2021-09-07)

* Fix following redirects if location is relative or schema-relative
* Provide the optional argument ?follow_redirect to not follow redirects

## v0.0.2 (2021-09-06)

* Unify response API between HTTP1 and HTTP2 requests and responses (issue #5)

## v0.0.1 (2021-09-06)

* Initial release
