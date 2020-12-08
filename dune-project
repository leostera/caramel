(lang dune 2.7)
(name caramel)

(generate_opam_files true)

(license Apache-2.0)
(source (github AbstractMachinesLab/caramel))

(maintainers "Leandro Ostera <leandro@ostera.io>")
(authors "Leandro Ostera <leandro@ostera.io>")

(using menhir 2.1)
(cram enable)

(package
 (name erlang)
 (synopsis "Libraries to manipulate Erlang sources")
 (description "erlang is a  a set of libraries designed to facilitate manipulating
 Standard Erlang and Core Erlang sources.

 It provides a lexer/parser, a concrete AST, and a printer for Standard Erlang
 in its current version.
")
 (depends
  (dune (>= "2.7"))
  (ocaml (>= "4.11.1"))
  ocaml-tree-sitter
  cmdliner
  menhir
  ppx_sexp_conv
  sexplib
  ))

(package
 (name caramel)
 (synopsis "An OCaml to Erlang compiler")
 (description "")
 (depends
  (dune (>= "2.7"))
  (ocaml (>= "4.11.1"))
  erlang
  menhir
  ppx_sexp_conv
  sexplib
  ))

(package
 (name tree-sitter)
 (synopsis "Bindings to the tree-sitter Runtime")
 (description "")
 (depends
  (dune (>= "2.7"))
  (ocaml (>= "4.11.1"))
  "conf-rust-2018"
  ))
