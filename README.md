# Caramel
> :candy: An Erlang backend to the OCaml compiler

## Getting started

You can bootstrap the system by running:

```sh
caramel $ make deps build
```

It relies on `opam` being installed, and `make deps` will take care of
installing all the necessary `opam` dependencies.

After `make` you should have a `caramelc` binary to play around with. 

## Hacking

The target `make watch` will start the Dune build watcher, and recompile the
project as you save files.

You can run tests with `make test`, and reformat the source code with `make fmt`.
