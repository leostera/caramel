# Erlang Compiler Backend for OCaml

## Getting started

You can bootstrap the system by running:

```sh
caramel $ ./configure
caramel $ make
caramel $ make caramelc
```

After that you should have a `caramelc` binary to play around with. For the time
being it has the same interface as `ocamlc`, which means that you need to 
pass it in `.ml` and `.mli` files _in the right order_.

You can use `caramelc -depent -sort *.ml *.mli` to get a sorted list or just run
this (assuming `caramelc` is in your `PATH`):

```sh
my_project $ caramelc -c $(caramelc -depend -sort *.ml *.mli)
```

## Rationale

OCaml is a much safer language than Erlang. It is also a lot more restricted.

A subset of OCaml should compile just fine to the functional parts of Erlang.

An interop library and some syntax extensions could bridge the gaps where OCaml
doesn't translate very well to Erlang.

## Mapping

This is a Typedtree mapping, which means we hook into the OCaml compiler right
after all of the type checks have been done.

The compiler will strive for zero-cost abstractions, so using OCaml idioms
incurs zero penalty on the final Erlang programs.

* Modules: Each OCaml module (not necessarily file) corresponds to a single
  Erlang module. This means that instance of module Functors will get
  monomorphized to single Erlang modules.

  This avoids the problem of having a runtime representation of OCaml functors
  backed into Erlang, so there's no overhead from it.

* Module Signatures: the signature of a module, will regulate what functions
  are exported from that module. If something does not appear on the signature,
  it will be kept unexported.

* Types: records map into records, variants map into tagged tuples.

  `(a, b) result = Ok of a | Error of b` maps into
  `-type result(A, B) :: {ok, A} | {error, B}`

# Differences from vanilla OCaml

Expect no mutability (yet), so `ref` and `mutable` should not be valid syntax
at some point.
