# Architecture

## Compiler

The Caramel compiler is composed of 2 main components: a Frontend and a Backend.

### Frontend: Parsing and Type Checking

The **Frontend** takes care parsing the OCaml sources and doing the
type-checking. You can find the entrypoint for the frontend here: [./caramel/compiler/compiler.ml](https://github.com/AbstractMachinesLab/caramel/tree/main/caramel/compiler/compiler.ml).

For the most part, the frontend takes care of picking the right modules to
parse the sources into the OCaml Parsetree, and it reuses most of the OCaml
compiler frontend to type-check the Parsetree into a Typedtree.

Once we have a Typedtree, we know for certain that the sources are well-typed,
and we can hand it over to the backend.

### Backend: Code Generation

The **Backend** takes care of generating the Erlang sources.

It has for input a Typedtree that is then translated into an Erlang AST, to be
pretty-printed into `.erl` sources.

The bulk of this translation happens in the
[./caramel/compiler/ocaml_to_erlang/](https://github.com/AbstractMachinesLab/caramel/tree/main/caramel/compiler/ocaml_to_erlang)
modules.
