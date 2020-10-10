# Roadmap

## 2020 Q4: [Milestone v0.1](https://github.com/AbstractMachinesLab/caramel/milestone/1)

The initial goal, to type-check Erlang, let me to the thesis that a subset of a
well-typed language like OCaml could be used to discover a well-typed subset of
Erlang, and that subset could then be successfully type-checked.

This yielded an OCaml to Erlang compiler as a byproduct, that is the focus of
the current milestone.

Caramel doesn't at the moment intend to support all of the existing OCaml
software, and not even all of the OCaml language. Only some of it, as it would
have to be a good citizen of the BEAM, allowing for interop with existing BEAM
languages. And it would have to be a subset of OCaml expressive enough to solve
meaningful, real-life problems, in a type-safe way.

This is why the **v0.1 milestone is focused on getting the compilation from
OCaml to Erlang to a good place**, where we can write type-safe application
cores in OCaml, and surround these with good old Erlang seamlessly.

To achieve this we will need to work primarily on:

* the translation from the OCaml Typedtree to the Erlang AST (See [Issue
  #6](https://github.com/AbstractMachinesLab/caramel/issues/6)),
* the checking of certain invariants of both ASTs (See [Issue
  #6](https://github.com/AbstractMachinesLab/caramel/issues/6)),
* the printing of the Erlang AST (See [Issue
  #7](https://github.com/AbstractMachinesLab/caramel/issues/7)), and on
* the runtime support required to execute the generated source code (See [Issue
  [#8](https://github.com/AbstractMachinesLab/caramel/issues/8)]).


### Future work

I'd like to build a foundation in the OCaml ecosystem to engage and benefit
from the BEAM, and I think this can be done by providing good libraries to work
with Erlang sources in OCaml programs.

This roughly means support for both Standard Erlang and Core Erlang:

* being able to read and parse sources with good error reports,

* providing AST definitions and helpers to construct, manipulate, and check
  different properites in them

* and printers to generate sources in a readable fashion

The work has already been started, as Caramel currently supports parsing a
subset of Erlang, and can produce Core Erlang sources for a very very small
subset of the lower level OCaml Lambda language.

On the other hand, having both an Erlang frontend _and_ backend to the OCaml
compiler means the BEAM can leverage from the broader OCaml ecosystem as well:

* Reason code (and other alternative OCaml syntaxes) could be transparently
  compiled to Erlang

* Erlang code could be compiled to Javascript with the Js_of_ocaml backend

* Erlang code that uses the OCaml standard library could be compiled to small
  native binaries running on the OCaml Runtime

* Erlang code could be compiled to Core Erlang ensuring type-safety

* Core Erlang code from any BEAM language could be type-checked as OCaml

* Type-driven tools for refactoring and verification could be used on Erlang
  sources

And many other ideas that have come up on discussions about this project so far.

The art is long but the life is short, so we'll do one thing at a time and
we'll see how far along we get.

/ Leandro
