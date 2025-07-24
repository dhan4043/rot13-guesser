(lang dune 3.12)
(name rot13_game)
(generate_opam_files true)
(package
 (name rot13_game)
 (depends
  (ocaml (>= 4.14))
  dune
  lwt
  lwt.unix
  cohttp-lwt-unix
  cohttp-lwt
  yojson
  uri
  str
  odoc))
