# Erlang Compiler Backend for OCaml

## Rationale

OCaml is a much safer language than Erlang.
It is also a lot more restricted.

A subset of OCaml should compile just fine to the functional parts of Erlang.

An interop library could bridge the gap to the concurrent parts of Erlang.

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
