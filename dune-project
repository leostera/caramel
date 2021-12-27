(lang dune 2.9)
(name caramel)

(generate_opam_files true)

(source (github AbstractMachinesLab/caramel))

(maintainers "Leandro Ostera <leandro@ostera.io>")
(authors "Leandro Ostera <leandro@ostera.io>")

(cram enable)

(package
 (name caramel)
 (synopsis "A functional language for building type-safe, scalable, and maintainable applications")
 (description "Caramel is a functional language for building type-safe, scalable, and maintainable applications.")
 (depends
  (dune (>= "2.9"))
  (ocamlformat (>= "0.17.0"))
  (ocaml (>= "4.13.0"))
  alcotest
  bos
  ppx_sexp_conv
  ppx_inline_test
  sexplib
  bisect_ppx
  fpath
  mtime
  ))
