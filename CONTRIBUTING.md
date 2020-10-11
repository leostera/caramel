# Contributing to Caramel :candy:

First of all, thanks for taking the time to contribute! :heart::tada::+1:

[See the Caramel architecture drawing](./docs/caramel_arch.jpg)

## Getting started

You can bootstrap the system by running:

```sh
caramel $ make deps build
```

It relies on `opam` being installed, and `make deps` will take care of
installing all the necessary `opam` dependencies.

The target `make watch` will start the Dune build watcher, and recompile the
project as you save files.

You can run tests with `make test`, and reformat the source code with `make
fmt`.

For a more in-depth description of what's happening, check the documentation of each compilation pipeline.

Unfortunately the compiler itself is not `dune install`-able yet, and if you
want to run it via `dune exec` you'll have to use the `--stdlib-path` flag to
point to the `_build/default/src/stdlib` for it to pick up the standard
library.

## Codebase

The Caramel codebase is structured to explore a few things:

* `./src/bin` -- the `caramelc` binary

* `./src/erlang` -- a library for working with Erlang and Core Erlang code in
  OCaml including lexers, parsers, ASTs, and invariant checkers

* `./src/compiler` -- includes the compilation pipelines

* `./src/typing` -- includes the type checking pipelines 

* `./src/stdlib` -- the standard libraries shipped with Caramel

