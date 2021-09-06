## Http-lwt-client -- simple HTTP client using http/af, h2, and lwt

A simple HTTP/1.1 and HTTP/2.0 client supporting HTTP and HTTPS. It follows
redirects (up to a given limit). Its dependency cone is small, and does not
include any wrappers or frameworks.

Additionally, a sample binary `hurl` is provided that exposes most of the
functionality as a command line program.

Other HTTP clients developed in OCaml include
[ofetch](https://github.com/cfcs/ofetch/) (minimal OCaml stdlib-only),
[piaf](https://github.com/anmonteiro/piaf) (HTTP 1 and HTTP 2 support),
[cohttp](https://github.com/mirage/ocaml-cohttp) (a functorised HTTP client and
server library), [curly](https://github.com/rgrinberg/curly) (a wrapper around
`curl`), [paf-le-chien](https://github.com/dinosaure/paf-le-chien) (a MirageOS
layer for HTTP/AF and H2) - but they didn't fit our needs.

This is distributed under the 3 clause BSD license, some code is inherited from
the [HTTP/AF](https://github.com/inhabitedtype/httpaf) project
(src/http_lwt_unix.{ml,mli} - as noted in that file).