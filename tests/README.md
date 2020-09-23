# A Test Suite for the Caramel Compiler

### Compiler Tests

This test suite is structured so that different, relatively orthogonal parts
of the language could be developed and tested in isolation.

If all of these pass, we should've covered the entire surface of compilation.

* `./erltest/expressions` includes tests for the different kind of ocaml expressions
  that are supported and how they translate to erlang.

* `./erltest/ffi` includes tests that exercise the foreign function interface
  capabilities of the compiler.

* `./erltest/functions` is for function expressions, lambdas, functions in cases,
  exported functions, etc.

* `./erltest/modules` includes tests that exercise handling of modules, including
  functors, signatures, nested modules, etc.

* `./erltest/types` is for types of all sorts: variants, records, function types,
  aliases, hidden types, abstract types, phantom types, etc.

### Other Experiments

These aren't necessarily compiler tests, but are useful to compile together since
they show more lifelike examples of code that would be interesting to write in
Caramel instead of vanilla Erlang.

* `./erltest/otp` includes attempts at typing the interface of a `gen_server`
  -- these are still pretty unsafe!

* `./erltest/processes` includes the current typings and examples for typed
  message passing across processes.

* `./erltest/realworld` is a collection of more life-like examples of code that
  can be compiled and run. This is either already existing OCaml code that we
  know the behavior of, or new OCaml code that resembles real-world code.
