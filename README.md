## Httpaf-lwt-client -- simple HTTP client using http/af and lwt

A simple HTTP/1.1 client supporting HTTP and HTTPS. It follows redirects (up
to a given limit). Its dependency cone is small, and does not include any
wrappers or frameworks.

Additionally, a sample binary `hurl` is provided that exposes most of the
functionality as a command line program.

Other HTTP clients developed in OCaml include
[piaf](https://github.com/anmonteiro/piaf),
[cohttp](https://github.com/mirage/ocaml-cohttp),
[curly](https://github.com/rgrinberg/curly) - but they didn't fit our needs.

This is distributed under the 3 clause BSD license, some code is inherited from
the [HTTP/AF](https://github.com/inhabitedtype/httpaf) project
(src/httpaf_lwt_unix.{ml,mli} - as noted in that file).