# Caramel
> :candy: An Erlang backend to the OCaml compiler

Core idea here is to grab a subset of OCaml, and turn it into valid, idiomatic
Erlang that you'd have hand written.

See: [./erlcomp/README.md](./erlcomp/README.md)

Check `./erltest` for some examples of how the sources are being compiled to erlang.

# Differences from vanilla OCaml

Expect no mutability (yet), so `ref` and `mutable` should not be valid syntax
at some point.
