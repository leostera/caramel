(lang dune 2.8)
(name caramel)

(generate_opam_files true)

(license Apache-2.0)
(source (github AbstractMachinesLab/caramel))

(maintainers "Leandro Ostera <leandro@ostera.io>")
(authors "Leandro Ostera <leandro@ostera.io>")

(using menhir 2.1)
(cram enable)

(package
 (name caramel)
 (synopsis "a functional language for building type-safe, scalable, and maintainable applications.")
 (description "Caramel is a functional language for building type-safe, scalable, and maintainable applications.")
 (depends
  (dune (>= "2.8"))
  (ocamlformat (= "0.16.0"))
  (ocaml (>= "4.11.1"))
  alcotest
  bos
  erlang
  menhir
  ppx_sexp_conv
  sexplib
  ))
