## v0.2.4 (2023-07-03)

* In HTTP/2, always lowercase the header keys (#19 by @ScoreUnder,
  fixes #7 by @kit-ty-kate)

## v0.2.3 (2023-03-20)

* Adapt to h2 0.10.0 API change (#18 by @hannesm)

## v0.2.2 (2023-02-17)

* Update to tls 0.16.0 package split: tls.lwt is now tls-lwt (#17 by @hannesm)

## v0.2.1 (2023-01-02)

* Fix ownership of buffers given by the HTTP scheduler (#16 by @dinosaure)

## v0.2.0 (2022-11-03)

* API: In the body function passed to request, the response is the first
  argument. This allows a client to do different stuff with the body, depending
  on the response code (and/or content type etc.) (#15 by @hannesm, reviewed and
  suggested by @reynir in https://git.robur.io/robur/http-mirage-client/pulls/2)

## v0.1.0 (2022-10-25)

* Add function "request", remove "one_request". The response body is a stream
  now to support responses where the body exceeds the memory (or maximum string
  length) (#14 @kit-ty-kate)
* BUGFIX hurl had "input" and "output" switched (#14 @hannesm)

## v0.0.8 (2022-09-17)

* use Status.is_redirection instead of a match on the polymorphic variant, to
  support unknown 3xx codes (such as 308) (#11 @hannesm)
* add some yield in the main loop to allow concurrency with other fibers (issue
  #12 by @kit-ty-kate, fixed in #13 by @dinosaure)
* hurl: add a no-follow flag (@hannesm)

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
